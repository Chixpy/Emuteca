unit utLEmuTKCacheSysIcons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Graphics, LazFileUtils,
  ucCHXImageList,
  ucEmutecaSystemManager, ucEmutecaSystem;

type

  { ctLEmuTKCacheSysIcons

    This Thread loads system icons in background.

  }

  ctLEmuTKCacheSysIcons = class(TThread)
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

{ ctLEmuTKCacheSysIcons }

procedure ctLEmuTKCacheSysIcons.SetDefSysIcon(AValue: TPicture);
begin
  if FDefSysIcon = AValue then
    Exit;
  FDefSysIcon := AValue;
end;

procedure ctLEmuTKCacheSysIcons.SetDefSoftIcon(AValue: TPicture);
begin
  if FDefSoftIcon = AValue then
    Exit;
  FDefSoftIcon := AValue;
end;

procedure ctLEmuTKCacheSysIcons.SetIconList(AValue: cCHXImageList);
begin
  if FIconList = AValue then
    Exit;
  FIconList := AValue;
end;

procedure ctLEmuTKCacheSysIcons.SetSystemManager(
  AValue: cEmutecaSystemManager);
begin
  if FSystemManager = AValue then
    Exit;
  FSystemManager := AValue;
end;

procedure ctLEmuTKCacheSysIcons.Execute;
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

constructor ctLEmuTKCacheSysIcons.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

end.
