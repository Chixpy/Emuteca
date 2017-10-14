unit ufLEmuTKIcnSysCBX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Types, StdCtrls, LCLType,
  ucCHXImageList,
  ucEmutecaSystem,
  ufEmutecaSystemCBX;

type

  { TfmLEmuTKIcnSysCBX }

  TfmLEmuTKIcnSysCBX = class(TfmEmutecaSystemCBX)
  private
    FSysIcons: cCHXImageList;
    procedure SetSysIcons(AValue: cCHXImageList);
  published
    property SysIcons: cCHXImageList read FSysIcons write SetSysIcons;

    procedure cbxSystemDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKIcnSysCBX }

procedure TfmLEmuTKIcnSysCBX.cbxSystemDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  RectIcon: TRect;
  aCBX: TComboBox;
  aSystem: cEmutecaSystem;

begin
  if odInactive in State then
    Exit;

  aCBX := TComboBox(Control);
  aSystem := cEmutecaSystem(aCBX.Items.Objects[Index]);


  // Icon
  // aCBX.Canvas.FillRect(ARect);
  RectIcon := ARect;
  RectIcon.Left := RectIcon.Left + 1;
  RectIcon.Right := RectIcon.Left + ARect.Bottom - ARect.Top;

  if assigned(aSystem) then
  begin
    if assigned(SysIcons) then
    begin
      if not assigned(aSystem.Stats.Icon) then
        aSystem.CacheIcon(SysIcons);

      if assigned(aSystem.Stats.Icon) then
        aCBX.Canvas.StretchDraw(RectIcon, aSystem.Stats.Icon.Graphic);
    end;
  end
  else
  begin
    // SysIcons[2] = Default system icon
    if SysIcons.Count > 2 then
    begin
      aCBX.Canvas.StretchDraw(RectIcon, SysIcons[2].Graphic);
    end;
  end;

  // Text
  aCBX.Canvas.TextOut(RectIcon.Right + 4, ARect.Top, aCBX.Items[Index]);
end;

procedure TfmLEmuTKIcnSysCBX.SetSysIcons(AValue: cCHXImageList);
begin
  if FSysIcons = AValue then
    Exit;
  FSysIcons := AValue;
end;

end.
