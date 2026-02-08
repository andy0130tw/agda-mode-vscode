// module for searching executables in PATH with tools like `which` or `where.exe`

module Error = {
  type t =
    | NotFound
    | InternalError
    | SomethingWentWrong(Connection__Process__Exec.Error.t)

  let toString = error =>
    switch error {
    | NotFound => "Command not found"
    | InternalError => "Internal error"
    | SomethingWentWrong(e) => Connection__Process__Exec.Error.toString(e)
    }
}

// Instead of returning the underlying error, we return `None` as a special case when `which` or `where.exe` is working correctly but the executable is not found.
let searchWith = async (command, name, ~timeout=1000) => {
  switch await Connection__Process__Exec.run(command, [name], ~timeout) {
  | Ok(stdout) =>
    let path = String.trim(stdout) // trim the string to remove the trailing newline
    Ok(path)
  | Error(FromStderr(Some(1), "")) => Error(Error.NotFound)
  | Error(FromStderr(Some(1), "INFO: Could not find files for the given pattern(s).\r\n")) =>
    Error(NotFound)
  | Error(error) => Error(SomethingWentWrong(error))
  }
}

let search = async (name, ~timeout=1000) => {
  let retryFor = (count: int, callback: () => promise<result<'a, 'b>>) => {
    let rec retry = async (n: int) =>
      switch await callback() {
      | Ok(v) => Ok(v)
      | Error(err) =>
        if n > 0 {
          await retry(n - 1)
        } else {
          Error(err)
        }
    }
    retry(count)
  }

  if OS.onUnix {
    await searchWith("which", name, ~timeout)
  } else {
    // try `which` first, then `where.exe`
    await retryFor(3, async () =>
      switch await searchWith("which", name, ~timeout) {
      | Ok(stdout) => if stdout == "" {
          Console.log("[XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX] 'which' returns empty result!!!")
          Error(Error.InternalError)
        } else { Ok(stdout) }
      | Error(_) => await searchWith("where.exe", name, ~timeout)
      }
    )
  }
}
