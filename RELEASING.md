# Releasing Predoc

Predoc releases are prepared locally, built by GitHub Actions and then
published from the draft GitHub release created by the release workflow.

Two different versions are used during this process:

- The **Predoc version** identifies the release, for example `0.2.6`.
- The **Janet version** selects the Janet release embedded in the browser
  runtime, for example `1.41.2`.

Commands below use those example versions. Substitute the versions required
for the release being prepared.

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
$ janet res/tools/version.janet 0.2.6
```

The script updates `info.jdn`, the Predoc manpage sources and the generated
mdoc manpages. Review and test the result:

```console
$ git diff
$ git diff --check
$ for f in test/*.janet; do janet "$f" || break; done
```

Stage only the version-related files. Do not accidentally include a locally
built `predoc` executable or other unrelated files:

```console
$ git add info.jdn man/man1/predoc.1 man/man1/predoc.1.predoc
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
$ janet res/tools/version.janet DEVEL
```

This updates `info.jdn`, the manpage sources and the generated manpages again.

## 6. Update the Browser Runtime

Pass the Janet language version—not the Predoc version—to the WebAssembly
build script. Docker is used by default:

```console
$ janet res/tools/wasm.janet 1.41.2
```

To use Podman:

```console
$ janet res/tools/wasm.janet 1.41.2 podman
```

Verify the generated assets with the same Janet version:

```console
$ node res/tools/wasm-smoke.mjs 1.41.2
$ python3 -m http.server --directory pages 8000
```

Open <http://localhost:8000/> and check the browser demo. Stop the server when
finished.

Review and commit the `DEVEL` reset, regenerated manpages, new JavaScript and
WebAssembly assets, and updated page references. Historically this follow-up
commit has been named:

```text
Update WebAssembly blob
```

Push the commit to deploy the updated `pages` directory:

```console
$ git push origin master
```
