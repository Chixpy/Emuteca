unit ucEmutecaSoftManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, IniFiles,
  uaEmutecaManager, ucEmutecaSoftware, ucEmutecaSystem;

resourcestring
  rsLoadingVersionList = 'Loading software list...';
  rsSavingVersionList = 'Saving software list...';

type

  { cEmutecaSoftManager }

  cEmutecaSoftManager = class(caEmutecaManager)
  private
    FVisibleList: cEmutecaSoftList;
    FFullList: cEmutecaSoftList;

  protected

  public
    procedure AssingAllTo(aList: TStrings); override;
    procedure AssingEnabledTo(aList: TStrings); override;
    property VisibleList: cEmutecaSoftList read FVisibleList;
    {< Filtered soft list }

    procedure LoadFromStrLst(TxtFile: TStrings); override;
    procedure SaveToStrLst(TxtFile: TStrings; const ExportMode: boolean);
      override;
    procedure LoadFromIni(aIniFile: TCustomIniFile); override;
    procedure SaveToIni(IniFile: TCustomIniFile; const ExportMode: boolean);
      override;

    function ItemById(aId: string;
      Autocreate: boolean = False): cEmutecaSoftware;
    {< Returns the software with aId key.

       @Result cEmutecaSoftware found or nil.
    }

    procedure SelectSystem(aSystem: cEmutecaSystem);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property FullList: cEmutecaSoftList read FFullList;
    {< Actual list where the software is stored. }
  end;

implementation

{ cEmutecaSoftManager }


function cEmutecaSoftManager.ItemById(aId: string;
  Autocreate: boolean): cEmutecaSoftware;
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  Result := nil;

  i := 0;
  while (Result = nil) and (i < FullList.Count) do
  begin
    aSoft := FullList[i];
    if UTF8CompareText(aSoft.ID, aId) = 0 then
      Result := aSoft;
    Inc(i);
  end;
end;

procedure cEmutecaSoftManager.SelectSystem(aSystem: cEmutecaSystem);
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  VisibleList.Clear;

  if not Assigned(aSystem) then
  begin
    VisibleList.Assign(FullList);
  end
  else
  begin
    i := 0;
    while (i < FullList.Count) do
    begin
      aSoft := FullList[i];
      if Assigned(aSoft.System) then
      begin
        if aSoft.System = aSystem then
          VisibleList.Add(aSoft);
      end
      else
      begin
        if UTF8CompareText(aSoft.SystemKey, aSystem.ID) = 0 then
        begin
          // Caching aSoft.System
          aSoft.System := aSystem;
          VisibleList.Add(aSoft);
        end;
      end;
      Inc(i);
    end;
  end;
end;

constructor cEmutecaSoftManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FFullList := cEmutecaSoftList.Create(True);
  FVisibleList := cEmutecaSoftList.Create(False);
  // TODO: OnCompare FullList.OnCompare := ;
end;

destructor cEmutecaSoftManager.Destroy;
begin
  FreeAndNil(FVisibleList);
  FreeAndNil(FFullList);
  inherited Destroy;
end;

procedure cEmutecaSoftManager.LoadFromIni(aIniFile: TCustomIniFile);
begin

end;

procedure cEmutecaSoftManager.SaveToIni(IniFile: TCustomIniFile;
  const ExportMode: boolean);
var
  aSoft: cEmutecaSoftware;
  i: integer;
begin
  if not Assigned(IniFile) then
    Exit;

  try
    i := 0;
    while i < FullList.Count do
    begin
      aSoft := FullList[i];
      aSoft.SaveToIni(IniFile, ExportMode);
      Inc(i);

      if Assigned(ProgressCallBack) then
        ProgressCallBack(rsSavingVersionList, aSoft.Folder,
          aSoft.FileName, i, FullList.Count);
    end;
  finally
    IniFile.UpdateFile;
  end;
end;

procedure cEmutecaSoftManager.AssingAllTo(aList: TStrings);
begin

end;

procedure cEmutecaSoftManager.AssingEnabledTo(aList: TStrings);
begin

end;

procedure cEmutecaSoftManager.LoadFromStrLst(TxtFile: TStrings);
var
  i: integer;
  TempSoft: cEmutecaSoftware;
begin
  if not Assigned(TxtFile) then
    Exit;

  // FullList.BeginUpdate;
  FullList.Capacity := FullList.Count + TxtFile.Count; // Speed Up?
  i := 1; // Skipping Header
  while i < TxtFile.Count do
  begin
    TempSoft := cEmutecaSoftware.Create(nil);
    TempSoft.DataString := TxtFile[i];

    FullList.Add(TempSoft);
    Inc(i);

    if ProgressCallBack <> nil then
      ProgressCallBack(rsLoadingVersionList, TempSoft.Folder,
        TempSoft.FileName, i, TxtFile.Count);
  end;
  // FullList.EndUpdate;

end;

procedure cEmutecaSoftManager.SaveToStrLst(TxtFile: TStrings;
  const ExportMode: boolean);
var
  i: integer;
  aSoft: cEmutecaSoftware;
begin
  if not Assigned(TxtFile) then
    Exit;

  { TODO : cEmutecaSoftManager.SaveToStrLst Export mode }
  TxtFile.Clear;
  TxtFile.BeginUpdate;
  try
    TxtFile.Capacity := FullList.Count + 1; // Speed up?
        TxtFile.Add('"System","Group","SHA1","ID","Folder","FileName",' +
        '"Title","TransliteratedName","SortTitle",' +
          '"Version","Year","Publisher","Zone",' +
          '"DumpStatus","DumpInfo","Fixed","Trainer","Translation",' +
          '"Pirate","Cracked","Modified","Hack",' +
          '"Last Time","Times Played","Playing Time"');


    i := 0;
    while i < FullList.Count do
    begin
      aSoft := FullList[i];
      TxtFile.Add(aSoft.DataString);
      Inc(i);

      if ProgressCallBack <> nil then
        ProgressCallBack(rsSavingVersionList, aSoft.Folder,
          aSoft.FileName, i, FullList.Count);
    end;

  finally
    TxtFile.EndUpdate;
  end;
end;

end.
