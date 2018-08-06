package cromwell

import java.net.URL
import java.nio.file.InvalidPathException

import better.files.File
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.validated._
import common.validation.ErrorOr.ErrorOr
import common.validation.Parse._
import common.validation.Validation._
import cromwell.CommandLineArguments._
import cromwell.CromwellApp.Command
import cromwell.core.path.{DefaultPathBuilder, Path}
import cromwell.webservice.PartialWorkflowSources
import cwl.preprocessor.CwlPreProcessor
import org.slf4j.Logger

import scala.util.{Failure, Success, Try}

object CommandLineArguments {
  val DefaultCromwellHost = new URL("http://localhost:8000")
  case class ValidSubmission(workflowSource: Option[String],
                             workflowUrl: Option[String],
                              workflowRoot: Option[String],
                              worflowInputs: String,
                              workflowOptions: String,
                              workflowLabels: String,
                              dependencies: Option[File])
}

case class CommandLineArguments(command: Option[Command] = None,
                                workflowSource: Option[String] = None,
                                workflowRoot: Option[String] = None,
                                workflowInputs: Option[Path] = None,
                                workflowOptions: Option[Path] = None,
                                workflowType: Option[String] = None,
                                workflowTypeVersion: Option[String] = None,
                                workflowLabels: Option[Path] = None,
                                imports: Option[Path] = None,
                                metadataOutput: Option[Path] = None,
                                host: URL = CommandLineArguments.DefaultCromwellHost
                               ) {
  private lazy val cwlPreProcessor = new CwlPreProcessor()
  private lazy val isCwl = workflowType.exists(_.equalsIgnoreCase("cwl"))

  /*
    * If the file is a relative local path, resolve it against the path of the input json.
   */
  private def inputFilesMapper(inputJsonPath: Path)(file: String) = {
    DefaultPathBuilder.build(file) match {
      case Success(path) if !path.isAbsolute => inputJsonPath.sibling(file).pathAsString
      case _ => file
    }
  }

  private def preProcessCwlInputFile(path: Path): ErrorOr[String] = {
    cwlPreProcessor.preProcessInputFiles(path.contentAsString, inputFilesMapper(path)).toErrorOr
  }

  def validateSubmission(logger: Logger): ErrorOr[ValidSubmission] = {
    def preProcessCwlWorkflowSource(workflowSourcePath: Path): ErrorOr[String] = {
      val workflowPath = File(workflowSourcePath.pathAsString)

      logger.info("Pre Processing Workflow...")
      lazy val preProcessedCwl = cwlPreProcessor.preProcessCwlFileToString(workflowPath, None)

      imports match {
        case Some(_) => readContent("Workflow source", workflowSourcePath)
        case None => Try(preProcessedCwl.value.unsafeRunSync())
          .toChecked
          .flatMap(identity)
          .toValidated
      }
    }

    def getWorkflowSourceFromPath(workflowPath: Path): ErrorOr[(Option[String], Option[String])] = {
      if (isCwl) preProcessCwlWorkflowSource(workflowPath).map(src => (Option(src), None))
      else (None, Option(workflowPath.pathAsString)).validNel
    }

    val workflowSourceAndUrl: ErrorOr[(Option[String], Option[String])] = DefaultPathBuilder.build(workflowSource.get) match {
      case Success(workflowPath) => {
        if (workflowPath.exists) getWorkflowSourceFromPath(workflowPath)
        else s"Workflow source path does not exist: $workflowPath".invalidNel
      }
      case Failure(e: InvalidPathException) => s"Invalid file path. Error: ${e.getMessage}".invalidNel
      case Failure(_) => PartialWorkflowSources.convertStringToUrl(workflowSource.get).map(validUrl => (None, Option(validUrl)))
    }

    val inputsJson: ErrorOr[String] = if (isCwl) {
      logger.info("Pre Processing Inputs...")
      workflowInputs.map(preProcessCwlInputFile).getOrElse(readOptionContent("Workflow inputs", workflowInputs))
    } else readOptionContent("Workflow inputs", workflowInputs)

    val optionsJson = readOptionContent("Workflow options", workflowOptions)
    val labelsJson = readOptionContent("Workflow labels", workflowLabels)

    val workflowImports: Option[File] = imports.map(p => File(p.pathAsString))

    (workflowSourceAndUrl, inputsJson, optionsJson, labelsJson) mapN {
      case ((w, u), i, o, l) =>
        ValidSubmission(w, u, workflowRoot, i, o, l, workflowImports)
    }
  }

  /** Read the path to a string. */
  private def readContent(inputDescription: String, path: Path): ErrorOr[String] = {
    if (!path.exists) {
      s"$inputDescription does not exist: $path".invalidNel
    } else if (!path.isReadable) {
      s"$inputDescription is not readable: $path".invalidNel
    } else path.contentAsString.validNel
  }

  /** Read the path to a string, unless the path is None, in which case returns "{}". */
  private def readOptionContent(inputDescription: String, pathOption: Option[Path]): ErrorOr[String] = {
    pathOption match {
      case Some(path) => readContent(inputDescription, path)
      case None => "{}".validNel
    }
  }
}
