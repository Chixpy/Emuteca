{ This file is part of Emuteca

  Copyright (C) 2006-2012 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{ Unit of Image Viewer form. }
unit fImageViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ActnList, ComCtrls, Spin, ExtCtrls, StdCtrls, Menus, Buttons,
  // Custom
  uConfig, uCHXStrUtils, uCHXImageUtils;

type

  { TfrmImageViewer }

  TfrmImageViewer = class(TForm)
    actClose: TAction;
    actFirst: TAction;
    actNext: TAction;
    actPrevious: TAction;
    actLast: TAction;
    actStretch: TAction;
    actOriginalSize: TAction;
    actZoomOut: TAction;
    actZoomIn: TAction;
    ActionList: TActionList;
    eCurrImage: TSpinEdit;
    ilActions: TImageList;
    Image: TImage;
    lTotalImages: TLabel;
    mClose: TMenuItem;
    mOriginalSize: TMenuItem;
    mZoomIn: TMenuItem;
    mZoomOut: TMenuItem;
    pmAction: TPopupMenu;
    sbInfo: TStatusBar;
    tbImage: TToolBar;
    bSep1: TToolButton;
    tbZoomIn: TToolButton;
    tbOriginalSize: TToolButton;
    tbZoomOut: TToolButton;
    tbClose: TToolButton;
    bSep2: TToolButton;
    bStretch: TToolButton;
    bFirst: TToolButton;
    bPrevious: TToolButton;
    bNext: TToolButton;
    bLast: TToolButton;
    bSep3: TToolButton;
    procedure actCloseExecute(Sender: TObject);
    procedure actFirstExecute(Sender: TObject);
    procedure actLastExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actOriginalSizeExecute(Sender: TObject);
    procedure actPreviousExecute(Sender: TObject);
    procedure actStretchExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure eCurrImageChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ImageDblClick(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageResize(Sender: TObject);

  private
    FConfig: cConfig;
    procedure SetConfig(const AValue: cConfig);

  protected
    ImageList: TStringList;

    DragBeginX: Longint;
    DragBeginY: Longint;

    procedure ChangeImage;
    procedure FixPosition;

  public
    property Config: cConfig read FConfig write SetConfig;

    procedure DisableStretch;
    procedure StretchImage;

    procedure AddImages(aImageList: TStrings; Index: Integer = 0);
    procedure AddImage(aImageFile: String);
  end;

var
  frmImageViewer: TfrmImageViewer;

implementation

{ TfrmImageViewer }

procedure TfrmImageViewer.FormCreate(Sender: TObject);
begin
  ImageList := TStringList.Create;
end;

procedure TfrmImageViewer.actZoomInExecute(Sender: TObject);
begin
  DisableStretch;

  // Trying to do the zoom around the center of the visible zone..
  // shr 1 = div 2
  if Image.Left <= 0 then
    Image.Left := Image.Left - Image.Width shr 1;
  if Image.Top <= 0 then
    Image.Top := Image.Top - Image.Height shr 1;

  Image.Height := Image.Height * 2;
  Image.Width := Image.Width * 2;
  FixPosition;
end;

procedure TfrmImageViewer.actOriginalSizeExecute(Sender: TObject);
begin
  DisableStretch;
  Image.Height := Image.Picture.Height;
  Image.Width := Image.Picture.Width;
  FixPosition;
end;

procedure TfrmImageViewer.actPreviousExecute(Sender: TObject);
begin
  if eCurrImage.Value > 1 then
    eCurrImage.Value :=  eCurrImage.Value - 1;
end;

procedure TfrmImageViewer.actStretchExecute(Sender: TObject);
begin
  // Inverted logic because actStretch is (un)checked before
  //   execute the action.
  if actStretch.Checked then
    StretchImage
  else
  begin
    DisableStretch;
    FixPosition;
  end;
end;

procedure TfrmImageViewer.actCloseExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmImageViewer.actFirstExecute(Sender: TObject);
begin
  eCurrImage.Value := 1;
  // ChangeImage; OnChange is called;
end;

procedure TfrmImageViewer.actLastExecute(Sender: TObject);
begin
  eCurrImage.Value := ImageList.Count;
  // ChangeImage; OnChange is called;
end;

procedure TfrmImageViewer.actNextExecute(Sender: TObject);
begin
  if eCurrImage.Value < ImageList.Count then
    eCurrImage.Value :=  eCurrImage.Value + 1;
end;

procedure TfrmImageViewer.actZoomOutExecute(Sender: TObject);
begin
  DisableStretch;
  // Trying to do the zoom around the center of the visible zone..
  // shr 2 = div 4
  if Image.Left <= 0 then
    Image.Left := Image.Left + Image.Width shr 2;
  if Image.Top <= 0 then
    Image.Top := Image.Top + Image.Height shr 2;
  Image.Height := Image.Height div 2;
  Image.Width := Image.Width div 2;
  FixPosition;
end;

procedure TfrmImageViewer.eCurrImageChange(Sender: TObject);
begin
  ChangeImage;
end;

procedure TfrmImageViewer.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ImageList);
end;

procedure TfrmImageViewer.FormResize(Sender: TObject);
begin
  FixPosition;
end;

procedure TfrmImageViewer.ImageDblClick(Sender: TObject);
begin
  self.Close;
end;

procedure TfrmImageViewer.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft:
    begin
      DragBeginX := X;
      DragBeginY := Y;
    end;
  end;
end;

procedure TfrmImageViewer.ImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft:
    begin
      Image.Left := Image.Left + X - DragBeginX;
      Image.Top := Image.Top + Y - DragBeginY;
      FixPosition;
    end;
  end;
end;

procedure TfrmImageViewer.ImageResize(Sender: TObject);
begin
  if Image.Align = alClient then
    sbInfo.Panels[0].Text := '(' + IntToStr(Image.Width) + 'x' +
      IntToStr(Image.Height) + ')';
end;

procedure TfrmImageViewer.SetConfig(const AValue: cConfig);
  procedure Translate;
  begin
      Self.Caption := Application.Title + ': ' + Self.Caption;
  end;

  //procedure TfrmImageViewer.SetConfig(const AValue: cConfig);
begin
  FConfig := AValue;

  Translate;

  // Iconos de las acciones
  ReadActionsIcons(Config.IconsIniFile, Self.Name, Config.ImagesFolder +
    Config.IconsSubfolder, ilActions, ActionList);
end;

procedure TfrmImageViewer.ChangeImage;
begin
  if ImageList.Count = 0 then
    Exit;

  if eCurrImage.Value > ImageList.Count then
    eCurrImage.Value := ImageList.Count;

  if FileExistsUTF8(ImageList[eCurrImage.Value - 1]) then
  begin
    Image.Picture.LoadFromFile(ImageList[eCurrImage.Value - 1]);
    sbInfo.Panels[1].Text := IntToStr(Image.Picture.Width) + 'x' +
      IntToStr(Image.Picture.Height);
    sbInfo.Panels[2].Text := ImageList[eCurrImage.Value - 1];
    StretchImage;
  end
  else
  begin
    ImageList.Delete(eCurrImage.Value);
    eCurrImage.MaxValue := ImageList.Count;
    ChangeImage;
  end;

  lTotalImages.Caption := '/ ' + IntToStr(ImageList.Count);
end;

procedure TfrmImageViewer.FixPosition;
begin
  // Horizontal position
  if Image.Width > self.ClientWidth then
  begin
    if -Image.Left > (Image.Width - self.ClientWidth) then
      Image.Left := -(Image.Width - self.ClientWidth);
    if Image.Left > 0 then
      Image.Left := 0;
  end
  else
  begin
    Image.Left := (self.ClientWidth - Image.Width) div 2;
  end;

  // Vertical position
  if Image.Height > self.ClientHeight - sbInfo.Height - tbImage.Height then
  begin
    if -Image.Top > (Image.Height - self.ClientHeight + sbInfo.Height) then
      Image.Top := -(Image.Height - self.ClientHeight + sbInfo.Height);
    if Image.Top > tbImage.Height then
      Image.Top := tbImage.Height;
  end
  else
  begin
    Image.Top := ((self.ClientHeight - tbImage.Height - Image.Height) div 2) +
      tbImage.Height;
  end;

  sbInfo.Panels[0].Text := IntToStr(Image.Width) + 'x' +
    IntToStr(Image.Height);
end;

procedure TfrmImageViewer.DisableStretch;
var
  factor: real;
begin
  if Image.Align <> alNone then
  begin
    // Factor of the stretched image
    factor := Image.Height / Image.Picture.Height;
    if factor < 1 then
    begin
      if factor < Image.Width / Image.Picture.Width then
        factor := Image.Height / Image.Picture.Height;
      factor := 1/(round(1/factor));
    end
    else
    begin
      if factor > Image.Width / Image.Picture.Width then
        factor := Image.Height / Image.Picture.Height;
      factor := round(factor);
    end;

    Image.Align := alNone;
    actStretch.Checked := False;

    Image.Height := round(image.Picture.Height * factor);
    Image.Width := round(image.Picture.Width * factor);
  end;
end;

procedure TfrmImageViewer.StretchImage;
begin
  if Image.Align <> alClient then
  begin
    Image.Align := alClient;
    actStretch.Checked := True;
    sbInfo.Panels[0].Text := '(' + IntToStr(Image.Width) + 'x' +
      IntToStr(Image.Height) + ')';
  end;
end;

procedure TfrmImageViewer.AddImages(aImageList: TStrings; Index: Integer = 0);
begin
  ImageList.AddStrings(aImageList);
  eCurrImage.MaxValue := ImageList.Count;
  eCurrImage.Value := Index + 1;
  ChangeImage;
end;

procedure TfrmImageViewer.AddImage(aImageFile: String);
begin
  ImageList.Add(aImageFile);
  eCurrImage.Value := 1;
  eCurrImage.MaxValue := ImageList.Count;
  ChangeImage;
end;

initialization
  {$I fImageViewer.lrs}

end.

