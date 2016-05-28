unit ucEmutecaVersion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazFileUtils, crc, sha1,
  uEmutecaCommon, uaEmutecaStorable,
  uCHXStrUtils;

type
  { cEmutecaVersion }

  cEmutecaVersion = class(TComponent)
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
    {< ID of the Parent (and Sorting)}
    property Folder: string read FFolder write SetFolder;
    {< Folder or archive where the file is in. }
    property FileName: string read FFileName write SetFileName;

  end;

  { cEmutecaVersionList }

  cEmutecaVersionList = class (specialize TFPGObjectList<cEmutecaVersion>)
  private
    FProgressCallBack: TEmutecaProgressCallBack;
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);
  public
    procedure SaveToFile(aFile: string);
    procedure LoadFromFile(aFile: string);

    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;
    //< CallBack function to show the progress in actions.
  end;

implementation

{ cEmutecaVersionList }

procedure cEmutecaVersionList.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then
    Exit;
  FProgressCallBack := AValue;
end;

procedure cEmutecaVersionList.SaveToFile(aFile: string);
var
  aVersion: cEmutecaVersion;
  aStringList: TStringList;
  i: integer;
begin
  if ExtractFileNameOnly(aFile) = '' then
    { TODO : Raise an exception }
    exit;
  aStringList := TStringList.Create;
  try
    i := 0;
    while i < Self.Count do
    begin
      aVersion := Self.items[i];
      aStringList.Add(aVersion.DataString);
      if ProgressCallBack <> nil then
        ProgressCallBack('Saving version list...', aVersion.Parent,
          aVersion.Title, i, aStringList.Count);
      Inc(i);
    end;
    aStringList.Sort;
    aStringList.Insert(0,'"ID","Parent","Title","Version","Folder","FileName"');
    aStringList.SaveToFile(aFile);
  finally
    FreeAndNil(aStringList);
  end;
end;

procedure cEmutecaVersionList.LoadFromFile(aFile: string);
var
  i: integer;
  aStringList: TStringList;
  aVersion: cEmutecaVersion;
begin
  if not FileExistsUTF8(aFile) then
    Exit;

  aStringList := TStringList.Create;
  try
    aStringList.LoadFromFile(aFile);
    if aStringList.Count > 0 then
      aStringList.Delete(0); // Removing header

    i := 0;
    while i < aStringList.Count do
    begin
      aVersion := cEmutecaVersion.Create(nil); // TODO: nil?
      self.Add(aVersion);

      // Load parent data
      aVersion.DataString := aStringList[i];

      if ProgressCallBack <> nil then
        ProgressCallBack('Loading version list...', aVersion.Parent,
          aVersion.Title, i, aStringList.Count);
      Inc(i);
    end;
  finally
    FreeAndNil(aStringList);
  end;
end;

{ cEmutecaVersion }

function cEmutecaVersion.GetDataString: string;
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    aStringList.Add(ID);
    aStringList.Add(System);
    aStringList.Add(Parent);
    aStringList.Add(Title);
    aStringList.Add(Description);
    aStringList.Add(Folder);
    aStringList.Add(FileName);

  finally
    Result := aStringList.CommaText;
    FreeAndNil(aStringList);
  end;
end;

procedure cEmutecaVersion.SetDataString(AValue: string);
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    aStringList.CommaText := AValue;

    if aStringList.Count > 0 then
      self.ID := aStringList[0];
    if aStringList.Count > 1 then
      self.System := aStringList[1];
    if aStringList.Count > 2 then
      self.Parent := aStringList[2];
    if aStringList.Count > 3 then
      self.Title := aStringList[3];
    if aStringList.Count > 4 then
      self.Description := aStringList[4];
    if aStringList.Count > 5 then
      self.Folder := aStringList[5];
    if aStringList.Count > 6 then
      self.FileName := aStringList[6];
  finally

    FreeAndNil(aStringList);
  end;
end;

procedure cEmutecaVersion.SetDescription(AValue: string);
begin
  if FDescription = AValue then
    Exit;
  FDescription := AValue;
end;

procedure cEmutecaVersion.SetFileName(AValue: string);
begin
  if FFileName = AValue then Exit;
  FFileName := AValue;
end;

procedure cEmutecaVersion.SetID(AValue: string);
begin
  if FID = AValue then
    Exit;
  FID := AValue;
end;

procedure cEmutecaVersion.SetParent(AValue: string);
begin
  if FParent = AValue then
    Exit;
  FParent := AValue;
end;

procedure cEmutecaVersion.SetFolder(AValue: string);
begin
  FFolder := SetAsFile(AValue);
end;

procedure cEmutecaVersion.SetSystem(AValue: string);
begin
  if FSystem=AValue then Exit;
  FSystem:=AValue;
end;

procedure cEmutecaVersion.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
end;

constructor cEmutecaVersion.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cEmutecaVersion.Destroy;
begin
  inherited Destroy;
end;

end.
