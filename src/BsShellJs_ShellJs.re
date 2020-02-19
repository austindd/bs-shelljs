type shellString('a);
type exit = [ `Exit ];
type array_ = [ `Array ];
type string_ = [ `String ];

[%%bs.raw {|
  function __isShellString__(obj) {
    if (
      obj
      && obj.cat
      && obj.exec
      && obj.grep
      && obj.sed
      && obj.uniq
    ) {
      return true;
    } else {
      return false;
    }
  }
|}];

module Pipe = {

  [@bs.send.pipe: shellString('a)] external toEnd: string => shellString('a) = "toEnd";

  [@bs.send.pipe: shellString('a)] external to_: string => shellString('a) = "to";
  [@bs.send.pipe: shellString('a)] external grep:
    (
      [@bs.unwrap] [
        | `string(string)
        | `regex(Js.Re.t)
      ],
    ) => shellString(string) = "grep";
  [@bs.send.pipe: shellString('a)] external grepWith:
    (
      [@bs.string] [
        | [@bs.as "-v"] `v
        | [@bs.as "-l"] `l
        | [@bs.as "-i"] `i
      ],
      [@bs.unwrap] [
        | `string(string)
        | `regex(Js.Re.t)
      ],
    ) => shellString(string) = "grep";

};

module ShellString = {
  type t('a) = shellString('a);
  type nonrec exit = exit;

  include Pipe;

  /**
   * Internally, these values can be null or undefined sometimes, usually
   * as some kind of "default" case before setting them based on command return
   * values. Here, we make some assumptions about what constitutes a "default"
   * value so we can have a monomorphic type.
   */

  /**
   * The exit code is always set to an integer by ShellJs after executing
   * a command. It can be null or undefined for a ShellString created via
   * the `new` operator. Thus, the case we are covering is initialization, and
   * `0` usually represents "success", so `0` should be a safe default value.
   */
  [@bs.get] external code: t('a) => Js.Null_undefined.t(int) = "code";
  let code: t('a) => int = (shellString) =>
    Js.Null_undefined.toOption(code(shellString)) |> fun
      | None => 0
      | Some(code_) => code_;

  [@bs.get] external stdout: t('a) => Js.Null_undefined.t(string) = "stdout";
  let stdout: t('a) => string = (shellString) =>
    Js.Null_undefined.toOption(stdout(shellString)) |> fun
      | None => ""
      | Some(out) => out;

  [@bs.get] external stderr: t('a) => Js.Null_undefined.t(string) = "stderr";
  let stderr: t('a) => string = (shellString) =>
    Js.Null_undefined.toOption(stderr(shellString)) |> fun
      | None => ""
      | Some(err) => err;

  module Unsafe = {
    [@bs.set] external setStdout: (t('a), string) => unit = "stdout";
    [@bs.set] external setStderr: (t('a), string) => unit = "stderr";
    [@bs.set] external setCode: (t('a), int) => unit = "code";

    external castToShellString_string: string => shellString(string) = "%identity";
    external castToShellString_array: array(string) => shellString(array(string)) = "%identity";
    external castToShellString_exit: 'a => shellString(exit) = "%identity";
    external castToString: shellString(string) => string = "%identity";
    external castToArray: shellString(array(string)) => array(string) = "%identity";

  };

  [@bs.val] external isShellString: (. 'a) => bool = "__isShellString__";

  [@bs.send] external toString: t(string) => string = "toString";
  [@bs.module "shelljs"] [@bs.new] external fromString: string => t(string) = "ShellString";
  let fromString = (str) => {
    open Unsafe;
    let ret = fromString(str);
    setCode(ret, 0);
    setStderr(ret, "");
    ret;
  };

  [@bs.val] [@bs.scope ("Array", "prototype", "slice")] external toArray: t(array(string)) => array(string) = "call";
  [@bs.module "shelljs"] [@bs.new] external fromArray: array(string) => t(array(string)) = "ShellString";
  let fromArray = (str) => {
    open Unsafe;
    let ret = fromArray(str);
    setCode(ret, 0);
    setStderr(ret, "");
    ret;
  };

  let fromList: list(string) => t(array(string)) = (lst) => Belt.List.toArray(lst) -> fromArray;
  let toList: t(array(string)) => list(string) = (shellString) => toArray(shellString) -> Belt.List.fromArray;

  let toResult: t('a) => Belt.Result.t(t('a), t('a)) = (shellString) =>
    switch (code(shellString)) {
      | 0 => Ok(shellString);
      | _ => Error(shellString);
    };

};

[@bs.module "shelljs"] external cat: string => shellString(string) = "cat";
[@bs.module "shelljs"] external cat_options: Js.t({..}) => string => shellString(string) = "cat";

[@bs.module "shelljs"] external catMany: array(string) => shellString(string) = "cat";
[@bs.module "shelljs"] external catMany_options: Js.t({..}) => array(string) => shellString(string) = "cat";

[@bs.module "shelljs"] external cd: unit => shellString(exit) = "cd";
[@bs.module "shelljs"] external cdPath: string => shellString(exit) = "cd";

[@bs.module "shelljs"] external chmod: (~mode: string, ~path: string) => shellString(exit) = "chmod";
[@bs.module "shelljs"] external chmod_options: (Js.t({..}), ~mode: string, ~path: string) => shellString(exit) = "chmod";

[@bs.module "shelljs"] external cp: (~source: string, ~dest: string) => shellString(exit) = "cp";
[@bs.module "shelljs"] external cp_options: (Js.t({..}), ~source: string, ~dest: string) => shellString(exit) = "cp";

[@bs.module "shelljs"] external pushd: unit => shellString(array(string)) = "pushd";
[@bs.module "shelljs"] external pushd_options: Js.t({..}) => shellString(array(string)) = "pushd";

[@bs.module "shelljs"] external pushdPath: string => shellString(array(string)) = "pushd";
[@bs.module "shelljs"] external pushdPath_options: (Js.t({..}), string) => shellString(array(string)) = "pushd";

[@bs.module "shelljs"] external popd: unit => shellString(array(string)) = "popd";
[@bs.module "shelljs"] external popd_options: Js.t({..}) => shellString(array(string)) = "popd";
[@bs.module "shelljs"] external popdN: string => shellString(array(string)) = "popd";
[@bs.module "shelljs"] external popdN_options: (Js.t({..}), string) => shellString(array(string)) = "popd";

[@bs.module "shelljs"] external dirs: unit => array(string) = "dirs";
let dirs = () => {
  open ShellString;
  let res = dirs();
  switch (isShellString(. res)) {
    | true => Unsafe.castToShellString_array(res);
    | false => fromArray(res);
  };
};

[@bs.module "shelljs"] external dirs_options: Js.t({..}) => array(string) = "dirs";
let dirs_options = (options) => {
  open ShellString;
  let res = dirs_options(options);
  switch (isShellString(. res)) {
    | true => Unsafe.castToShellString_array(res);
    | false => fromArray(res);
  };
};

[@bs.module "shelljs"] external dirsN: string => string = "dirs";
let dirsN = (arg) => {
  open ShellString;
  let res = dirsN(arg);
  switch (isShellString(. res)) {
    | true => Unsafe.castToShellString_string(res);
    | false => fromString(res);
  };
};

[@bs.module "shelljs"] external dirsN_options: Js.t({..}) => string => string = "dirs";
let dirsN_options = (options, arg) => {
  open ShellString;
  let res = dirsN_options(options, arg);
  switch (isShellString(. res)) {
    | true => Unsafe.castToShellString_string(res);
    | false => fromString(res);
  };
};

[@bs.module "shelljs"] external echo: string => shellString(string) = "echo";
[@bs.module "shelljs"] external echo_options: Js.t({..}) =>string => shellString(string) = "echo";

type execCallback = (~code: int, ~stdout: string, ~stderr: string) => unit;
[@bs.module "shelljs"] external exec: string => shellString(exit) = "exec";
[@bs.module "shelljs"] external exec_options: (string, Js.t({..})) => shellString(exit) = "exec";
[@bs.module "shelljs"] external execCallback: string => execCallback => unit = "exec";
[@bs.module "shelljs"] external execCallback_options: (string, Js.t({..})) => execCallback => unit = "exec";

[@bs.module "shelljs"] external find: string => shellString(array(string)) = "find";
[@bs.module "shelljs"] external findMany: array(string) => shellString(array(string)) = "find";

[@bs.module "shelljs"] external grepString: (string, ~path: string) => shellString(string) = "grep";
[@bs.module "shelljs"] external grepString_options: (Js.t({..}), string, ~path: string) => shellString(string) = "grep";
[@bs.module "shelljs"] external grepRegex: (Js.Re.t, ~path: string) => shellString(string) = "grep";
[@bs.module "shelljs"] external grepRegex_options: (Js.t({..}), Js.Re.t, ~path: string) => shellString(string) = "grep";
[@bs.module "shelljs"] external grepStringMany: (string, ~path: string) => shellString(string) = "grep";
[@bs.module "shelljs"] external grepStringMany_options: (Js.t({..}), string, ~path: string) => shellString(string) = "grep";
[@bs.module "shelljs"] external grepRegexMany: (Js.Re.t, ~path: string) => shellString(string) = "grep";
[@bs.module "shelljs"] external grepRegexMany_options: (Js.t({..}), Js.Re.t, ~path: string) => shellString(string) = "grep";

[@bs.module "shelljs"] external head: string => shellString(string) = "head";
[@bs.module "shelljs"] external head_options: (Js.t({..})) => string => shellString(string) = "head";

[@bs.module "shelljs"] external ln: (~source: string, ~dest: string) => shellString(string) = "ln";
[@bs.module "shelljs"] external ln_options: (Js.t({..}), ~source: string, ~dest: string) => shellString(string) = "ln";

[@bs.module "shelljs"] external ls: unit => shellString(array(string)) = "ls";
[@bs.module "shelljs"] external ls_options: (Js.t({..})) => shellString(array(string)) = "ls";
[@bs.module "shelljs"] external lsPath: string => shellString(array(string)) = "ls";
[@bs.module "shelljs"] external lsPath_options: (Js.t({..}), ~path: string,) => shellString(array(string)) = "ls";

[@bs.module "shelljs"] external mkdir: string => shellString(exit) = "mkdir";
[@bs.module "shelljs"] external mkdir_options: (Js.t({..}), string) => shellString(exit) = "mkdir";
[@bs.module "shelljs"] external mkdirMany: array(string) => shellString(exit) = "mkdir";
[@bs.module "shelljs"] external mkdirMany_options: (Js.t({..}), array(string)) => shellString(exit) = "mkdir";

[@bs.module "shelljs"] external mv: (~source: string, ~dest: string) => shellString(exit) = "mv";
[@bs.module "shelljs"] external mv_options: (Js.t({..}), ~source: string, ~dest: string) => shellString(exit) = "mv";
[@bs.module "shelljs"] external mvMany: (~source: array(string), ~dest: string) => shellString(exit) = "mv";
[@bs.module "shelljs"] external mvMany_options: (Js.t({..}), ~source: array(string), ~dest: string) => shellString(exit) = "mv";

[@bs.module "shelljs"] external pwd: unit => shellString(string) = "pwd";

[@bs.module "shelljs"] external rm: string => shellString(exit) = "rm";
[@bs.module "shelljs"] external rm_options: Js.t({..}) =>string => shellString(exit) = "rm";
[@bs.module "shelljs"] external rmMany: array(string) => shellString(exit) = "rm";
[@bs.module "shelljs"] external rmMany_options: Js.t({..}) => array(string) => shellString(exit) = "rm";

[@bs.module "shelljs"] external test: (
    [@bs.string] [
      | [@bs.as "-b"] `b
      | [@bs.as "-c"] `c
      | [@bs.as "-d"] `d
      | [@bs.as "-e"] `e
      | [@bs.as "-f"] `f
      | [@bs.as "-L"] `L
      | [@bs.as "-p"] `p
      | [@bs.as "-S"] `S
    ],
    string
  ) => bool = "test";



