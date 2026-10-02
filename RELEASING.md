# Releasing Predoc

Predoc releases are prepared locally, built by GitHub Actions and then
published from the draft GitHub release created by the release workflow.

The commands below use `0.2.6` as an example Predoc version. Substitute the
version required for the release being prepared.

## 1. Check the Development Branch

Ensure all intended changes have been committed and push `master`:

```console
$ git status
$ git push origin master
```

Wait for the test workflow on GitHub Actions to pass before preparing the
release commit.

## 2. Prepare the Release Commit

Pass the Predoc version without its `v` prefix to the version script:

```console
$ wattle res/tools/version.wattle 0.2.6
```

The script updates `info.edn`, the Predoc manpage sources and the generated
mdoc manpages. Review and test the result:

```console
$ git diff
$ git diff --check
$ wattle test
```

Stage only the version-related files. Do not accidentally include a locally
built `predoc` executable or other unrelated files:

```console
$ git add info.edn man/man1/predoc.1 man/man1/predoc.1.predoc
$ git add man/man7/predoc.7 man/man7/predoc.7.predoc
$ git commit -m "Prepare for v0.2.6 release"
$ git push origin master
```

Wait for the test workflow to pass again.

## 3. Tag the Release

Predoc uses lightweight Git tags. Add the `v` prefix when creating the tag:

```console
$ git tag v0.2.6
$ git push origin v0.2.6
```

## 4. Build the Release Archives

Run the `release` workflow with the tag as its version input:

```console
$ gh workflow run release.yml -f version=v0.2.6
```

Alternatively, open GitHub Actions, select the `release` workflow, choose
**Run workflow**, and enter `v0.2.6`.

The workflow checks out the tag, runs the tests, builds archives for the
supported platforms and creates a draft GitHub release containing those
archives. When the workflow succeeds, review the draft release and publish it.

## 5. Return to Development

After publishing the release, reset the source version to `DEVEL`:

```console
$ wattle res/tools/version.wattle DEVEL
```

This updates `info.edn`, the manpage sources and the generated manpages again.

Commit the reset and push `master`:

```console
$ git add info.edn man/man1/predoc.1 man/man1/predoc.1.predoc
$ git add man/man7/predoc.7 man/man7/predoc.7.predoc
$ git commit -m "Reset version to DEVEL"
$ git push origin master
```

## 6. Browser Runtime

The browser demo in `pages` has not been ported to Wattle, so there is nothing
to rebuild at release time. See the README.
