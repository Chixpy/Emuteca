unit ucEmutecaVersionManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8,
  uaEmutecaManager, ucEmutecaVersion;

resourcestring
  rsLoadingVersionList = 'Loading version list...';
  rsSavingVersionList = 'Saving version list...';

type

  { cEmutecaVersionManager }

  cEmutecaVersionManager = class(caEmutecaManagerTxt)
  private
    FFullList: cEmutecaVersionList;

  protected


  public
    property FullList: cEmutecaVersionList read FFullList;
    {< Actual list where the parents are stored. }

    procedure LoadFromFileTxt(TxtFile: TStrings); override;
    procedure SaveToFileTxt(TxtFile: TStrings; const ExportMode: boolean);
      override;

    function ItemById(aId: string): cEmutecaVersion;
    {< Returns the version with aId key.

       @Result cEmutecaParent found or nil.
    }

    procedure AssingAllTo(aList: TStrings); override;
    procedure AssingEnabledTo(aList: TStrings); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ cEmutecaVersionManager }



function cEmutecaVersionManager.ItemById(aId: string): cEmutecaVersion;
var
  i: integer;
begin
  //// FullList.TryGetData(aId, Result); Maybe do this???

  //Result := nil;
  //i := FullList.IndexOf(aId);

  //if i >= 0 then
  //  Result := FullList.Data[i];
end;
procedure cEmutecaVersionManager.AssingAllTo(aList: TStrings);
var
  i: longint;
begin
  if not assigned(aList) then
    aList := TStringList.Create;

  aList.BeginUpdate;
  i := 0;
  while i < FullList.Count do
  begin
    aList.AddObject(FullList[i].Title, FullList[i]);
    Inc(i);
  end;
  aList.EndUpdate;
end;

procedure cEmutecaVersionManager.AssingEnabledTo(aList: TStrings);
begin
  { TODO : Maybe search for enabled systems... }
  AssingAllTo(aList);
end;

constructor cEmutecaVersionManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FFullList := cEmutecaVersionList.Create(True);
  // TODO: OnCompare FullList.OnCompare := ;
end;

destructor cEmutecaVersionManager.Destroy;
begin
  FreeAndNil(FFullList);
  inherited Destroy;
end;

procedure cEmutecaVersionManager.LoadFromFileTxt(TxtFile: TStrings);
var
  i: integer;
  TempVersion: cEmutecaVersion;
begin
  if not Assigned(TxtFile) then
    Exit;

  i := 1; // Skipping Header
  while i < TxtFile.Count do
  begin
    TempVersion := cEmutecaVersion.Create(nil);
    TempVersion.DataString := TxtFile[i];
    FullList.Add(TempVersion);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsLoadingVersionList, TempVersion.Title,
        TempVersion.Description, i, TxtFile.Count);
  end;
end;

procedure cEmutecaVersionManager.SaveToFileTxt(TxtFile: TStrings;
  const ExportMode: boolean);
var
  i: integer;
begin
  if not Assigned(TxtFile) then
    Exit;

  if not ExportMode then
  begin
    TxtFile.Clear;
    TxtFile.Add('"ID","System","Parent","Title","Version","Folder","FileName"');
  end;

  i := 0;
  while i < FullList.Count do
  begin
    TxtFile.Add(FullList[i].DataString);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsSavingVersionList, FullList[i].System,
        FullList[i].Title, i + 1, FullList.Count);
    Inc(i);
  end;
end;

end.
