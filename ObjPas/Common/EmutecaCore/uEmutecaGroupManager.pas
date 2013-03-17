unit uEmutecaGroupManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uEmutecaGameGroup, uEmutecaGame,
  uGenericGroupManager;

type
  cEmutecaGroupManager = specialize cGenericGroupManager<string,
    cEmutecaGameGroup, string, cEmutecaGame>;

implementation

end.

