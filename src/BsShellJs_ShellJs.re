type shellString('a);
type result;

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
  type nonrec result = result;

  include Pipe;

  [@bs.module "shelljs"] [@bs.new] external fromString: string => t(string) = "ShellString";
  [@bs.send] external toString: t(string) => string = "toString";

  [@bs.module "shelljs"] [@bs.new] external fromArray: array(string) => t(array(string)) = "ShellString";
  [@bs.val] [@bs.scope ("Array", "prototype", "slice")] external toArray: t(array(string)) => array(string) = "call";

  let fromList: list(string) => t(array(string)) = (lst) => Belt.List.toArray(lst) -> fromArray;
  let toList: t(array(string)) => list(string) = (shellString) => toArray(shellString) -> Belt.List.fromArray;

  [@bs.get] external code: t('a) => int = "code";
  [@bs.get] external stdout: t('a) => string = "stdout";
  [@bs.get] external stderr: t('a) => string = "stderr";

};

[@bs.module "shelljs"] external cat: string => shellString('a) = "cat";
[@bs.module "shelljs"] external catMany: array(string) => shellString('a) = "cat";

[@bs.module "shelljs"] external catWith_obj_n: {. "-n": int } => string => shellString('a) = "cat";
[@bs.module "shelljs"] external catManyWith_obj_n: {. "-n": int } => array(string) => shellString('a) = "cat";

[@bs.module "shelljs"] external cd: (~path: string=?, unit) => shellString(string) = "cd";

[@bs.module "shelljs"] external chmod: (~mode: string, ~path: string) => shellString(string) = "chmod";
[@bs.module "shelljs"] external chmodWith:
  (
    [@bs.string] [
      | [@bs.as "-v"] `v
      | [@bs.as "-c"] `c
      | [@bs.as "-R"] `R
    ],
    ~mode: string,
    ~path: string
  ) => shellString(string) = "chmod";

[@bs.module "shelljs"] external cp: (~source: string, ~dest: string) => shellString(string) = "cp";

let testCp = cp(~source="./test-dir/test1.txt", ~dest="./test-dir/test4.txt");
Js.log(testCp);

let testCp = cp(~source="./test-dir/test5.txt", ~dest="./test-dir/test4.txt");
Js.log(testCp);

[@bs.module "shelljs"] external ln: (~source: string, ~dest: string) => shellString(string) = "ln";
[@bs.module "shelljs"] external lnWith:
  (
    [@bs.string] [
      | [@bs.as "-s"] `s
      | [@bs.as "-f"] `f
      | [@bs.as "-sf"] `sf
    ],
    ~source: string,
    ~dest: string,
  ) => shellString(string) = "ln";

[@bs.module "shelljs"] external ls: unit => shellString(array(string)) = "ls";
[@bs.module "shelljs"] external lsWith: (
  ~options: [@bs.string] [
    | [@bs.as "-R"] `R
    | [@bs.as "-A"] `A
    | [@bs.as "-L"] `L
    | [@bs.as "-D"] `d
    | [@bs.as "-l"] `l
  ],
) => shellString(string) = "ls";

[@bs.module "shelljs"] external lsPath: string => shellString(array(string)) = "ls";
[@bs.module "shelljs"] external lsPathWith: (
  ~options: [@bs.string] [
    | [@bs.as "-R"] `R
    | [@bs.as "-A"] `A
    | [@bs.as "-L"] `L
    | [@bs.as "-D"] `d
    | [@bs.as "-l"] `l
  ],
  string,
) => shellString(string) = "ls";

[@bs.module "shelljs"] external head: string => shellString(string) = "head";
[@bs.module "shelljs"] external headWith_obj_n: {. "-n": int } => string => shellString(string) = "head";

// lsPath("../../") -> ShellString.toList -> Js.log;
// lsPath("../../") -> ShellString.toArray -> Js.log;

// Pipe.(lsPath("./test-dir", ()) |> grep(`string(".txt"))) |> shellStringoString |> Js.log;

// let test2 = cd(()) -> ShellString.concat(ShellString.fromString("Hi")) -> Js.log;

// headWith({ "-n": 20 }, "./BsShellJs_ShellJs.re") -> Js.log;

