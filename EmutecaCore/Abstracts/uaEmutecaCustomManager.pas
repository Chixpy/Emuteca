unit uaEmutecaCustomManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uaCHXStorable;

type

  { caEmutecaCustomManager }

  caEmutecaCustomManager = class(caCHXStorableTxt)
    procedure ImportFromFileIni(aIniFile: TMemIniFile); virtual;
    procedure ImportFromIni(aIniFile: TMemIniFile); virtual; abstract;
    procedure ImportFromFileStrLst(aTxtFile: TStrings); virtual;
    procedure ImportFromStrLst(aTxtFile: TStrings); virtual; abstract;
  end;

implementation

{ caEmutecaCustomManager }

procedure caEmutecaCustomManager.ImportFromFileIni(aIniFile: TMemIniFile);
var
  aIniFile: TMemIniFile;
  IniFileOps: TIniFileOptions;
begin
  if aFilename = '' then
    aFilename := IniFileName;
  if not FileExistsUTF8(aFilename) then
    Exit;

  try
    aIniFile := TMemIniFile.Create(UTF8ToSys(aFilename));
    IniFileOps :=  aIniFile.Options;
    Exclude(IniFileOps, ifoCaseSensitive);
    aIniFile.Options := IniFileOps;

    ImportFromFileIni(aIniFile);
  finally
    FreeAndNil(aIniFile);
  end;
end;

procedure caEmutecaCustomManager.ImportFromFileStrLst(aTxtFile: TStrings);
var
  aTxtFile: TStringList;
begin
  if aFilename = '' then
    aFilename := TxtFileName;
  if not FileExistsUTF8(aFilename) then
    Exit;

  try
    aTxtFile := TStringList.Create;
    aTxtFile.LoadFromFile(UTF8ToSys(aFilename));
    aTxtFile.CaseSensitive := False;

    ImportFromStrLst(aTxtFile);
  finally
    FreeAndNil(aTxtFile);
  end;
end;

end.

