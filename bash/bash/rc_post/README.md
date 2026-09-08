# Bash RC Post Hooks

This directory contains hook snippets sourced after the main interactive-shell setup. It is where most day-to-day shell ergonomics live.

## Contents

* [`.present`](.present) is a marker file used by the surrounding bash configuration layout.
* [`aliases`](aliases) defines shell aliases.
* [`aws`](aws) defines AWS profile and S3 inventory/audit helpers.
* [`git-prompt`](git-prompt) and [`git-worktrees`](git-worktrees) add git-aware shell behavior.
* [`prompt`](prompt) configures the command prompt.
* [`rsync`](rsync) defines rsync-related helpers.

## AWS helper dependencies

The helpers in [`aws`](aws) require an AWS CLI release whose `s3api
list-buckets` output includes `BucketRegion` (as current AWS CLI v2 releases
do), authenticated AWS credentials, and network access to AWS.  The caller
needs permission for the operations it invokes:

* `sts:GetCallerIdentity`
* `s3:ListAllMyBuckets`
* `s3:GetAccountPublicAccessBlock`
* `s3:GetBucketPublicAccessBlock`
* `s3:GetBucketPolicyStatus`

`aws-s3-list` and `aws-s3-audit` also use `column`, normally supplied by the
Linux `util-linux` package.  The audit requires Bash 4.3 or newer for `wait -n`.
It makes two AWS requests per bucket and queries up to eight buckets in
parallel by default; set `AWS_S3_AUDIT_JOBS` to a positive integer to change
that limit.

`aws-s3-account-block` must address an S3 Control regional endpoint even
though account-level Block Public Access applies globally.  It selects a
region in this order: its optional positional argument, `AWS_REGION`,
`AWS_DEFAULT_REGION`, the active AWS profile's configured region, and finally
`us-east-1`.
