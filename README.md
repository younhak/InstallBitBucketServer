
It installs R Package in BitBucket Dev Server of Nielsen Data Science.
It is based on `devtools` package, with a few tweaks.

## Installation

You can install the customized version of `devtools` for
`install_bitbucket_server()` from my personal GitHub with:

``` r
install.packages("devtools")
devtools::install_github("younhak/InstallBitbucketServer")
```

## Pre-requisite

### Create Personal Token in Bitbucket

User Guide of Atlassian:
[link](https://confluence.atlassian.com/bitbucketserver057/personal-access-tokens-945543529.html?utm_campaign=in-app-help&utm_medium=in-app-help&utm_source=stash#Personalaccesstokens-usingpersonalaccesstokens)

1.  Access your Bitbucket **DEV** server
2.  Click Avatar on your right top edge
3.  Click \[Manage account\] in drill-down menu
4.  Click \[Personal access tokens\] in right hand side navigation bar
5.  Click \[Create a token\] button
6.  Enther the \[Token name\] (R, your name, and so on)
7.  Click \[Create\] butten
8.  **\[IMPORTANT\]** Copy and Paste your token in safe place *(You will
    not be able to view this token again)*
9.  Click \[Continue\]

![Figure 1. Token
Creation](man/figures/token.png)

## Usage

``` r
# Please don't import this package, because of conflict with original devtools
InstallBitBucketServer::install_bitbucket_server(projects = "project_name", 
                                                 repos = "repository_name", 
                                                 host = "host_server_name")  # host is "https://hostserver.com/bitbucket"
```
