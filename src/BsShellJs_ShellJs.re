module ShellString = {
  type canPipe = [ `canPipe ];
  type string_ = [ `string ];
  type array_ = [ `array ];
  type t('a);

  module Base = {
    [@bs.send.pipe: t('a)] external toEnd: string => t('a) = "toEnd";
    [@bs.send.pipe: t('a)] external to_: string => t('a) = "to";
    [@bs.send.pipe: t('a)] external concat: t('a) => t('a) = "concat";
    [@bs.get] external length: t('a) => int = "length";
  };

  module String = {
    type nonrec t = t(string_);
    module Impl = {
      include Base;
      external fromJsString: string => t = "%identity";
      external toJsString: t => string = "%identity";
      [@bs.send.pipe: t] external endsWith: t => bool = "endsWith";
    };
    include Impl;
  };

  module Array = {
    type nonrec t = t(array_);
    module Impl = {
      include Base;
      external fromJsArray: array(string) => t = "%identity";
      external toJsArray: t => array(string) = "%identity";
    };
    include Impl;
  };

  include String.Impl;
  include Array.Impl;


};

[@bs.module "shelljs"] external cat: string => ShellString.t('a) = "cat";
[@bs.module "shelljs"] external catMany: array(string) => ShellString.t([ `array ]) = "cat";
[@bs.module "shelljs"] external cd: (~path: string=?) => ShellString.t('a) = "cd";
[@bs.module "shelljs"] external ln:
  (
    ~options: [@bs.string] [
      | [@bs.as "-s"] `s
      | [@bs.as "-f"] `f
      | [@bs.as "-sf"] `sf
    ]=?,
    ~source: string,
    ~dest: string
  ) => ShellString.t([ `string ]) = "ln";

let x = ln(~options = `s, ~dest="here", ~source="there");

