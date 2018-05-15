package cromwell.backend.impl.bcs

import cromwell.backend.io.JobPaths
import cromwell.core.JobKey
import cromwell.core.path.{DefaultPathBuilder, Path}

object BcsJobPaths {
	val BcsLogPathKey = "bcsLog"
	val BcsEnvExecKey = "exec"
	val BcsEnvCwdKey = "cwd"
	val BcsEnvStdoutKey = "stdout"
	val BcsEnvStderrKey = "stderr"
	val BcsCommandDirectory: Path = DefaultPathBuilder.get("/cromwell_root")
	val BcsTempInputDirectory: Path = DefaultPathBuilder.get("/cromwell_inputs")
	val BcsStdoutRedirectPath = "bcs-stdout"
	val BcsStderrRedirectPath = "bcs-stderr"
}

final case class BcsJobPaths(workflowPaths: BcsWorkflowPaths, jobKey: JobKey) extends JobPaths {

	import BcsJobPaths._

	// alibaba cloud's batchcompute service can only support tar.gz formatted package.
	val workerFileName = "worker.tar.gz"
	val workerPath = callRoot.resolve(workerFileName)
	val bcsStdoutPath = callRoot.resolve(BcsStdoutRedirectPath)
	val bcsStderrPath = callRoot.resolve(BcsStderrRedirectPath)
}
