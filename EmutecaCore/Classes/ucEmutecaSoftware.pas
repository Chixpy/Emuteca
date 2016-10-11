unit ucEmutecaSoftware;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazFileUtils, contnrs,
  uaEmutecaStorable,
  uCHXStrUtils;

type
  { cEmutecaSoftware }

  cEmutecaSoftware = class(caEmutecaStorableTxt)
  private
    FDescription: string;
    FFileName: string;
    FID: string;
    FParent: string;
    FFolder: string;
    FSystem: string;
    FTitle: string;
    function GetDataString: string;
    procedure SetDataString(AValue: string);
    procedure SetDescription(AValue: string);
    procedure SetFileName(AValue: string);
    procedure SetID(AValue: string);
    procedure SetParent(AValue: string);
    procedure SetFolder(AValue: string);
    procedure SetSystem(AValue: string);
    procedure SetTitle(AValue: string);

  public
    property DataString: string read GetDataString write SetDataString;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFileTxt(TxtFile: TStrings); override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      override;

  published
    property ID: string read FID write SetID;
    {< ID of the file. Usually SHA1 }
    property System: string read FSystem write SetSystem;
    {< ID of the System. }
    property Parent: string read FParent write SetParent;
    {< ID of the parent. }
    property Title: string read FTitle write SetTitle;
    {< Title. }
    property Description: string read FDescription write SetDescription;
    {< Version info, hack, good dump}
    property Folder: string read FFolder write SetFolder;
    {< Folder or archive where the file is in. }
    property FileName: string read FFileName write SetFileName;

  end;

  { cEmutecaSoftList }

  cEmutecaSoftList = TComponentList;

implementation

{ cEmutecaSoftware }

function cEmutecaSoftware.GetDataString: string;
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    SaveToFileTxt(aStringList, false);
  finally
    Result := aStringList.CommaText;
    FreeAndNil(aStringList);
  end;
end;

procedure cEmutecaSoftware.SetDataString(AValue: string);
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    aStringList.CommaText := AValue;

    LoadFromFileTxt(aStringList);
  finally
    FreeAndNil(aStringList);
  end;
end;

procedure cEmutecaSoftware.SetDescription(AValue: string);
begin
  if FDescription = AValue then
    Exit;
  FDescription := AValue;
end;

procedure cEmutecaSoftware.SetFileName(AValue: string);
begin
  FFileName := SetAsFile(AValue);
end;

procedure cEmutecaSoftware.SetID(AValue: string);
begin
  FID := SetAsID(AValue);
end;

procedure cEmutecaSoftware.SetParent(AValue: string);
begin
  FParent := SetAsID(AValue);
end;

procedure cEmutecaSoftware.SetFolder(AValue: string);
begin
  FFolder := SetAsFolder(AValue);
end;

procedure cEmutecaSoftware.SetSystem(AValue: string);
begin
  FSystem := SetAsID(AValue);
end;

procedure cEmutecaSoftware.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
end;

constructor cEmutecaSoftware.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cEmutecaSoftware.Destroy;
begin
  inherited Destroy;
end;

procedure cEmutecaSoftware.LoadFromFileTxt(TxtFile: TStrings);
begin
  if not assigned(TxtFile) then
    Exit;
  if TxtFile.Count > 0 then
    self.ID := TxtFile[0];
  if TxtFile.Count > 1 then
    self.System := TxtFile[1];
  if TxtFile.Count > 2 then
    self.Parent := TxtFile[2];
  if TxtFile.Count > 3 then
    self.Title := TxtFile[3];
  if TxtFile.Count > 4 then
    self.Description := TxtFile[4];
  if TxtFile.Count > 5 then
    self.Folder := TxtFile[5];
  if TxtFile.Count > 6 then
    self.FileName := TxtFile[6];
end;

procedure cEmutecaSoftware.SaveToFileTxt(TxtFile: TStrings;
  const ExportMode: boolean);
begin
  if not assigned(TxtFile) then
    Exit;

  TxtFile.Add(ID);
  TxtFile.Add(System);
  TxtFile.Add(Parent);
  TxtFile.Add(Title);
  TxtFile.Add(Description);
  TxtFile.Add(Folder);
  TxtFile.Add(FileName);
end;

end.
