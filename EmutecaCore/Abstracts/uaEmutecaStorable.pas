unit uaEmutecaStorable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazUTF8, LazFileUtils;

type

  { caEmutecaStorable }

  caEmutecaStorable = class(TComponent)
  private
    FDataFile: string;
    procedure SetDataFile(AValue: string);

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(FileName: string); virtual; abstract;
    {< Loads data from file.

         @param(FileName Path to the file.)
    }
    procedure SaveToFile(FileName: string; const ExportMode: boolean);
      virtual; abstract;
    {< Saves data to file.

         @param(FileName Path to the file.)
         @param(ExportMode if @true don't save user data.)
    }
  published
    property DataFile: string read FDataFile write SetDataFile;
    {< File for read/write data by default. }

  end;

  { caEmutecaStorableIni }

  caEmutecaStorableIni = class(caEmutecaStorable)
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(FileName: string); override;
    procedure LoadFromFileIni(IniFile: TCustomIniFile); virtual; abstract;
    {< Loads data from file.

         @param(IniFile Inifile to read from.)
    }

    procedure SaveToFile(FileName: string; const ExportMode: boolean);
      override;
    procedure SaveToFileIni(IniFile: TCustomIniFile;
      const ExportMode: boolean); virtual; abstract;
     {< Saves data to file.

         @param(IniFile Inifile to write to.)
         @param(ExportMode if @true don't save user data.)
    }
  end;

  { caEmutecaStorableTxt }

  caEmutecaStorableTxt = class(caEmutecaStorable)
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(FileName: string); override;
    procedure LoadFromFileTxt(TxtFile: TStrings); virtual; abstract;
    {< Loads data from file.

         @param(TxtFile Text file to read from.)
    }

    procedure SaveToFile(FileName: string; const ExportMode: boolean);
      override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      virtual; abstract;
     {< Saves data to file.

         @param(TxtFile Text file to write to.)
         @param(ExportMode if @true don't save user data.)
    }
  end;

implementation

{ caEmutecaStorableTxt }

constructor caEmutecaStorableTxt.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caEmutecaStorableTxt.Destroy;
begin
  inherited Destroy;
end;

procedure caEmutecaStorableTxt.LoadFromFile(FileName: string);
var
  aTxtFile: TStringList;
begin
  if FileName = '' then
    FileName := DataFile;
  if not FileExistsUTF8(FileName) then
    Exit;

  aTxtFile := TStringList.Create;
  aTxtFile.LoadFromFile(UTF8ToSys(FileName));
  aTxtFile.CaseSensitive := False;
  try
    LoadFromFileTxt(aTxtFile);
  finally
    FreeAndNil(aTxtFile);
  end;
end;

procedure caEmutecaStorableTxt.SaveToFile(FileName: string;
  const ExportMode: boolean);
var
  aTxtFile: TStringList;
begin
  if FileName = '' then
    FileName := DataFile;
  if FileName = '' then
    Exit;
  aTxtFile := TStringList.Create;

  { TODO : caEmutecaStorableTxt.SaveToFile Export mode }
  if ExportMode and FileExistsUTF8(FileName) then
    aTxtFile.LoadFromFile(UTF8ToSys(FileName));

  aTxtFile.CaseSensitive := False;
  try
    SaveToFileTxt(aTxtFile, ExportMode);
    aTxtFile.SaveToFile(UTF8ToSys(FileName));
  finally
    FreeAndNil(aTxtFile);
  end;
end;

{ caEmutecaStorable }

procedure caEmutecaStorable.SetDataFile(AValue: string);
begin
  if FDataFile = AValue then
    Exit;
  FDataFile := AValue;
end;

constructor caEmutecaStorable.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caEmutecaStorable.Destroy;
begin
  inherited Destroy;
end;

{ caEmutecaStorableIni }

constructor caEmutecaStorableIni.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caEmutecaStorableIni.Destroy;
begin
  inherited Destroy;
end;

procedure caEmutecaStorableIni.LoadFromFile(FileName: string);
var
  aIniFile: TMemIniFile;
begin
  if FileName = '' then
    FileName := DataFile;
  if not FileExistsUTF8(FileName) then
    Exit;

  aIniFile := TMemIniFile.Create(UTF8ToSys(FileName));
  aIniFile.CaseSensitive := False;
  try
    LoadFromFileIni(aIniFile);
  finally
    FreeAndNil(aIniFile);
  end;
end;

procedure caEmutecaStorableIni.SaveToFile(FileName: string;
  const ExportMode: boolean);
var
  aIniFile: TMemIniFile;
begin
  if FileName = '' then
    FileName := DataFile;
  if FileName = '' then
    exit;
  aIniFile := TMemIniFile.Create(UTF8ToSys(FileName));
  aIniFile.CaseSensitive := False;
  try
    SaveToFileIni(aIniFile, ExportMode);
    aIniFile.UpdateFile;
  finally
    FreeAndNil(aIniFile);
  end;
end;

end.
