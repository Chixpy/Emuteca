unit uaEmutecaCustomManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazFileUtils, LazUTF8,
  uaCHXStorable,
  uEmutecaCommon;

type

  { caEmutecaCustomManager }

  caEmutecaCustomManager = class(caCHXStorableTxt)
  private
    FProgressCallBack: TEmutecaProgressCallBack;

  protected
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);

  public
    property ProgressCallBack: TEmutecaProgressCallBack read FProgressCallBack write SetProgressCallBack;

    procedure ImportFromFileIni(aFilename: string); virtual;
    procedure ImportFromIni(aIniFile: TIniFile); virtual; abstract;
    procedure ImportFromFileCSV(aFilename: string); virtual;
    procedure ImportFromStrLst(aTxtFile: TStrings); virtual; abstract;
  end;

implementation

{ caEmutecaCustomManager }

procedure caEmutecaCustomManager.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then Exit;
  FProgressCallBack := AValue;
end;

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

procedure caEmutecaCustomManager.ImportFromFileCSV(aFilename: string);
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

