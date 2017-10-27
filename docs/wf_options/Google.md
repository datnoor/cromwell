Valid keys and their meanings:

- **jes_gcs_root** - (Pipelines API backend only) Specifies where outputs of the workflow will be written.  Expects this to be a GCS URL (e.g. `gs://my-bucket/workflows`).  If this is not set, this defaults to the value within `backend.jes.config.root` in the [Configuration](Configuring).
- **google_compute_service_account** - (Pipelines API backend only) Specifies an alternate service account to use on the compute instance (e.g. my-new-svcacct@my-google-project.iam.gserviceaccount.com).  If this is not set, this defaults to the value within `backend.jes.config.genomics.compute-service-account` in the [Configuration](Configuring) if specified or `default` otherwise.
- **google_project** - (Pipelines API backend only) Specifies which google project to execute this workflow.
- **refresh_token** - (Pipelines API backend only) Only used if `localizeWithRefreshToken` is specified in the [Configuration file](Configuring).
- **auth_bucket** - (Pipelines API backend only) defaults to the the value in **jes_gcs_root**.  This should represent a GCS URL that only Cromwell can write to.  The Cromwell account is determined by the `google.authScheme` (and the corresponding `google.userAuth` and `google.serviceAuth`)
- **monitoring_script** - (Pipelines API backend only) Specifies a GCS URL to a script that will be invoked prior to the user command being run.  For example, if the value for monitoring_script is "gs://bucket/script.sh", it will be invoked as `./script.sh > monitoring.log &`.  The value `monitoring.log` file will be automatically de-localized.