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
    FDefaultIcon: TPicture;
    FIconList: cCHXImageList;
    FSystemManager: cEmutecaSystemManager;
    procedure SetDefaultIcon(AValue: TPicture);
    procedure SetIconList(AValue: cCHXImageList);
    procedure SetSystemManager(AValue: cEmutecaSystemManager);

  protected
    procedure Execute; override;

  public
    property DefaultIcon: TPicture read FDefaultIcon write SetDefaultIcon;
    property IconList: cCHXImageList read FIconList write SetIconList;
    property SystemManager: cEmutecaSystemManager
      read FSystemManager write SetSystemManager;

    constructor Create;
  end;

implementation

{ ctLEmuTKCacheSysIcons }

procedure ctLEmuTKCacheSysIcons.SetDefaultIcon(AValue: TPicture);
begin
  if FDefaultIcon = AValue then
    Exit;
  FDefaultIcon := AValue;
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
begin
  if not assigned(SystemManager) then
    Exit;
  if not assigned(IconList) then
    Exit;
  // if not assigned(DefaultIcon) then Exit; // Can be nil

  i := 0;
  while (not Terminated) and (i < SystemManager.FullList.Count) do
  begin
    aSystem := SystemManager.FullList[i];

    if FileExistsUTF8(aSystem.IconFile) then
    begin
      if Terminated then
        Exit;
      aSystem.Stats.Icon := IconList[IconList.AddImageFile(aSystem.IconFile)];
    end
    else
    begin
      if Terminated then
        Exit;
      aSystem.Stats.Icon := DefaultIcon;
    end;

    Inc(i);
  end;
end;

constructor ctLEmuTKCacheSysIcons.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

end.
