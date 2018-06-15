
# InstallBitbucketServer

It installs R Package in BitBucket Dev Server of Nielsen Data Science.
It is based on `devtools` package, with a few tweaks.

## Installation

You can install the customized version of `devtools` for
`install_bitbucket_server()` from my personal GitHub with:

``` r
install.packages("devtools")
devtools::install_github("younhak/InstallBitbucketServer")
```

## Usage

``` r
# Please don't import this package, because of confliction with original devtools
# Just use NAMESPACE::function()
InstallBitBucketServer::install_bitbucket_server(
  projects = "project_name", 
  repos = "repository_name", 
  host = "host_server_name"  # host is "https://hostserver.com/bitbucket"
)
```
