unit uaEmutecaCustomManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  uaCHXStorable, LazFileUtils, LazUTF8;

type

  { caEmutecaCustomManager }

  caEmutecaCustomManager = class(caCHXStorableTxt)
  public
    procedure ImportFromFileIni(aFilename: string); virtual;
    procedure ImportFromIni(aIniFile: TMemIniFile); virtual; abstract;
    procedure ImportFromFileStrLst(aFilename: string); virtual;
    procedure ImportFromStrLst(aTxtFile: TStrings); virtual; abstract;
  end;

implementation

{ caEmutecaCustomManager }

procedure caEmutecaCustomManager.ImportFromFileIni(aFilename: string);
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

    ImportFromIni(aIniFile);
  finally
    FreeAndNil(aIniFile);
  end;
end;

procedure caEmutecaCustomManager.ImportFromFileStrLst(aFilename: string);
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

