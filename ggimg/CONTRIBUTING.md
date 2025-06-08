# Contributing

Contributions, including bug fixes and new features, to
cleanNLP are welcome. When contributing to this repository,
please first discuss the change you wish to make via issue,
email, or any other method with the owners of this repository
before making a change. Small bug fixes can be given directly
as pull requests.

Please note we have a code of conduct, please follow it in
all your interactions with the project.

## Pull Request Process

1. Ensure that `R CMD CHECK --as-cran` passes. Note that the
   tests in the repository silently skip if they do not find
   the spaCy or CoreNLP libraries. Make sure you have both
   installed if you would like to contribute.
2. Increase the version number in the DESCRIPTION file. If
   you would like to be listed as a contributor in the
   DESCRIPTION, please discuss this prior to submitting the
   pull request.
