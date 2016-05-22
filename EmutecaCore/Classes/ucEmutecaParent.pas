unit ucEmutecaParent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazFileUtils,
  uEmutecaCommon;

type
  { cEmutecaParent }

  cEmutecaParent = class(TComponent)
  private
    FSortName: string;
    FSystem: string;
    FTitle: string;
    function GetDataString: string;
    procedure SetDataString(AValue: string);
    procedure SetSortName(AValue: string);
    procedure SetSystem(AValue: string);
    procedure SetTitle(AValue: string);


  public
    property DataString: string read GetDataString write SetDataString;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Title: string read FTitle write SetTitle;
    {< Name of the parent. }
    property System: string read FSystem write SetSystem;
    {< ID of the system. }
    property SortName: string read FSortName write SetSortName;
    {< ID of the Parent (and Sorting)}
  end;

  { cEmutecaParentList }

  cEmutecaParentList = class (specialize TFPGObjectList<cEmutecaParent>)
  private
    FProgressCallBack: TEmutecaProgressCallBack;
    procedure SetProgressCallBack(AValue: TEmutecaProgressCallBack);
  public
    procedure LoadFromFile(aFile: string);

    procedure SaveToFile(aFile: string);

    property ProgressCallBack: TEmutecaProgressCallBack
      read FProgressCallBack write SetProgressCallBack;
    //< CallBack function to show the progress in actions.
  end;

implementation

{ cEmutecaParentList }

procedure cEmutecaParentList.SetProgressCallBack(
  AValue: TEmutecaProgressCallBack);
begin
  if FProgressCallBack = AValue then
    Exit;
  FProgressCallBack := AValue;
end;

procedure cEmutecaParentList.LoadFromFile(aFile: string);
var
  i: integer;
  aStringList: TStringList;
  aParent: cEmutecaParent;
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
      aParent := cEmutecaParent.Create(nil); // TODO: nil?
      self.Add(aParent);

      // Load parent data
      aParent.DataString := aStringList[i];

      if ProgressCallBack <> nil then
        ProgressCallBack('Loading parent list...', aParent.System,
          aParent.Title, i, aStringList.Count);
      Inc(i);
    end;
  finally
    FreeAndNil(aStringList);
  end;
end;

procedure cEmutecaParentList.SaveToFile(aFile: string);
var
  aParent: cEmutecaParent;
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
      aParent := Self.items[i];
      aStringList.Add(aParent.DataString);
      if ProgressCallBack <> nil then
        ProgressCallBack('Saving parent list...', aParent.SortName,
          aParent.Title, i, aStringList.Count);
      Inc(i);
    end;
    aStringList.Sort;
     aStringList.Insert(0,'"ID/Sort Name","System","Title"');
    aStringList.SaveToFile(aFile);
  finally
    FreeAndNil(aStringList);
  end;
end;

{ cEmutecaParent }

procedure cEmutecaParent.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
end;

procedure cEmutecaParent.SetSystem(AValue: string);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;
end;

procedure cEmutecaParent.SetSortName(AValue: string);
begin
  if FSortName = AValue then
    Exit;
  FSortName := AValue;
end;

function cEmutecaParent.GetDataString: string;
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    aStringList.Add(SortName);
    aStringList.Add(System);
    aStringList.Add(Title);

  finally
    Result := aStringList.CommaText;
    FreeAndNil(aStringList);
  end;
end;

procedure cEmutecaParent.SetDataString(AValue: string);
var
  aStringList: TStringList;
begin
  aStringList := TStringList.Create;
  try
    aStringList.CommaText := AValue;

    if aStringList.Count > 0 then
      self.SortName := aStringList[0];
    if aStringList.Count > 1 then
      self.System := aStringList[1];
    if aStringList.Count > 2 then
      self.Title := aStringList[2];
  finally

    FreeAndNil(aStringList);
  end;
end;

constructor cEmutecaParent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cEmutecaParent.Destroy;
begin
  inherited Destroy;
end;

end.
