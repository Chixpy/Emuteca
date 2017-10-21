unit utLEmuTKCacheGrpIcons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LazFileUtils,
  ucCHXImageList,
  ucEmutecaGroupList, ucEmutecaGroup;

type

  { ctLEmuTKCacheGrpIcons

    This Thread loads group icons in background.
  }

  ctLEmuTKCacheGrpIcons = class(TThread)
  private
    FDefaultIcon: TPicture;
    FGroupList: cEmutecaGroupList;
    FIconList: cCHXImageList;
    FImageExt: TStrings;
    procedure SetDefaultIcon(AValue: TPicture);
    procedure SetGroupList(AValue: cEmutecaGroupList);
    procedure SetIconList(AValue: cCHXImageList);
    procedure SetImageExt(AValue: TStrings);

  protected
    procedure Execute; override;

  public
    property ImageExt: TStrings read FImageExt write SetImageExt;
    property GroupList: cEmutecaGroupList read FGroupList write SetGroupList;
    property DefaultIcon: TPicture read FDefaultIcon write SetDefaultIcon;
    property IconList: cCHXImageList read FIconList write SetIconList;

    constructor Create;
  end;

implementation

{ ctLEmuTKCacheGrpIcons }

procedure ctLEmuTKCacheGrpIcons.SetDefaultIcon(AValue: TPicture);
begin
  if FDefaultIcon = AValue then
    Exit;
  FDefaultIcon := AValue;
end;

procedure ctLEmuTKCacheGrpIcons.SetGroupList(AValue: cEmutecaGroupList);
begin
  if FGroupList = AValue then
    Exit;
  FGroupList := AValue;
end;

procedure ctLEmuTKCacheGrpIcons.SetIconList(AValue: cCHXImageList);
begin
  if FIconList = AValue then
    Exit;
  FIconList := AValue;
end;

procedure ctLEmuTKCacheGrpIcons.SetImageExt(AValue: TStrings);
begin
  if FImageExt = AValue then Exit;
  FImageExt := AValue;
end;

procedure ctLEmuTKCacheGrpIcons.Execute;
var
  i: integer;
  aGroup: cEmutecaGroup;
  TempStr: string;
begin
  if not assigned(GroupList) then
    Exit;
  if not assigned(IconList) then
    Exit;
  if not assigned(DefaultIcon) then
    Exit; // This can't be nil

  i := 0;
  while (not Terminated) and (i < GroupList.Count) do
  begin
    aGroup := GroupList[i];

    if not Assigned(aGroup.Stats.Icon) then
    begin
      if Terminated then
        Exit;
      TempStr := aGroup.SearchFirstRelatedFile(aGroup.CachedSystem.IconFolder,
        ImageExt, True);

      if FileExistsUTF8(TempStr) then
      begin
        if Terminated then
          Exit;
        aGroup.Stats.Icon := IconList[IconList.AddImageFile(TempStr)];
      end
      else
      begin
        if Terminated then
          Exit;
        aGroup.Stats.Icon := DefaultIcon;
      end;
    end;
    Inc(i);
  end;
end;

constructor ctLEmuTKCacheGrpIcons.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

end.
