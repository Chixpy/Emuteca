unit utETKGUICacheSysIcons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Graphics, LazFileUtils,
  ucCHXImageList,
  ucEmutecaSystemManager, ucEmutecaSystem;

type

  { ctEGUICacheSysIcons

    This Thread loads system icons in background.

  }

  ctEGUICacheSysIcons = class(TThread)
  private
    FDefSoftIcon: TPicture;
    FDefSysIcon: TPicture;
    FIconList: cCHXImageList;
    FSystemManager: cEmutecaSystemManager;
    procedure SetDefSoftIcon(AValue: TPicture);
    procedure SetDefSysIcon(AValue: TPicture);
    procedure SetIconList(AValue: cCHXImageList);
    procedure SetSystemManager(AValue: cEmutecaSystemManager);

  protected
    procedure Execute; override;

  public
    property DefSysIcon: TPicture read FDefSysIcon write SetDefSysIcon;
    property DefSoftIcon: TPicture read FDefSoftIcon write SetDefSoftIcon;
    property IconList: cCHXImageList read FIconList write SetIconList;
    property SystemManager: cEmutecaSystemManager
      read FSystemManager write SetSystemManager;

    constructor Create;
  end;

implementation

{ ctEGUICacheSysIcons }

procedure ctEGUICacheSysIcons.SetDefSysIcon(AValue: TPicture);
begin
  if FDefSysIcon = AValue then
    Exit;
  FDefSysIcon := AValue;
end;

procedure ctEGUICacheSysIcons.SetDefSoftIcon(AValue: TPicture);
begin
  if FDefSoftIcon = AValue then
    Exit;
  FDefSoftIcon := AValue;
end;

procedure ctEGUICacheSysIcons.SetIconList(AValue: cCHXImageList);
begin
  if FIconList = AValue then
    Exit;
  FIconList := AValue;
end;

procedure ctEGUICacheSysIcons.SetSystemManager(
  AValue: cEmutecaSystemManager);
begin
  if FSystemManager = AValue then
    Exit;
  FSystemManager := AValue;
end;

procedure ctEGUICacheSysIcons.Execute;
var
  i: integer;
  aSystem: cEmutecaSystem;
  aIcon: TPicture;
begin
  if not assigned(SystemManager) then
    Exit;
  if not assigned(IconList) then
    Exit;
  // if not assigned(DefSysIcon) then Exit; // Can be nil
  // if not assigned(DefSoftIcon) then Exit; // Can be nil

  i := 0;
  while (not Terminated) and (i < SystemManager.FullList.Count) do
  begin
    aSystem := SystemManager.FullList[i];

    if FileExistsUTF8(aSystem.IconFile) then
    begin
      if Terminated then
        Exit;
      aIcon := IconList[IconList.AddImageFile(aSystem.IconFile)];
    end
    else
    begin
      aIcon := DefSysIcon;
    end;

    if Terminated then
      Exit;
    aSystem.Stats.Icon := aIcon;

    if FileExistsUTF8(aSystem.SoftIconFile) then
    begin
      if Terminated then
        Exit;
      aIcon := IconList[IconList.AddImageFile(aSystem.SoftIconFile)];
    end
    else
    begin
      aIcon := DefSoftIcon;
    end;

    if Terminated then
      Exit;
    aSystem.Stats.SysSoftIcon := aIcon;

    Inc(i);
  end;
end;

constructor ctEGUICacheSysIcons.Create;
begin
  inherited Create(True);
  DefSoftIcon := nil; // Just to be sure...
  DefSysIcon := nil;
  FreeOnTerminate := True;
end;

end.
