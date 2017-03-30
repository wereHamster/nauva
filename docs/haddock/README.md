Files in this folder are used to generate [Haddock] documentation for the core
nauva packages.

> https://storage.googleapis.com/nvdocs/latest/index.html

 - `stack.yaml`: defines the packages which are included in the documentation
 - `deploy.yaml`: Google Cloud Container Builder configuration file which
   builds the documentation and uploads it into the `nvdocs` bucket.
 - `Dockerfile`: Docker image which contains GHC, stack and all dependencies
   required by the nauva packages. It is used to speed up the builds.
 - `setup.yaml`: Google Cloud Container Builder configuration file which
   builds the docker image.

To manually submit the job, use the `gcloud` command from the [Google Cloud SDK].
Though that shouldn't be needed, there is a build trigger which does that
automatically after each push to the repository.

    gcloud container builds submit --config deploy.yaml ../..

One-time setup to create the GHC/stack docker image:

    gcloud container builds submit --config setup.yaml ../..

[Haddock]: https://www.haskell.org/haddock/
[Google Cloud SDK]: https://cloud.google.com/sdk/
