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

{TfrmConfigManager unit}
unit fConfigManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, EditBtn, Buttons, uConfig;

type

  { TfrmConfigManager }

  TfrmConfigManager = class(TForm)
    bAceptar: TBitBtn;
    bCancel: TBitBtn;
    bMakePathsRelative: TButton;
    e7zPath: TFileNameEdit;
    e7zGPath: TFileNameEdit;
    emPlayerPath: TFileNameEdit;
    gbxImageExt: TGroupBox;
    gbxCompExt: TGroupBox;
    gbxTextExt: TGroupBox;
    l7zPath: TLabel;
    l7zGPath: TLabel;
    lConfigFile: TLabel;
    lmPlayerPath: TLabel;
    mCompExt: TMemo;
    mTextExt: TMemo;
    mImageExt: TMemo;
    p7zPath: TPanel;
    p7zGPath: TPanel;
    pmPlayerPath: TPanel;
    pBottom: TPanel;
    pcConfig: TPageControl;
    pagExtensions: TTabSheet;
    pagPaths: TTabSheet;
    procedure bAceptarClick(Sender: TObject);
    procedure bMakePathsRelativeClick(Sender: TObject);

  private
    { private declarations }
    FConfig: cConfig;
    procedure SetConfig(AValue: cConfig);

  public
    { public declarations }
    property Config: cConfig read FConfig write SetConfig;

  end;

var
  frmConfigManager: TfrmConfigManager;

implementation

{ TfrmConfigManager }

procedure TfrmConfigManager.bMakePathsRelativeClick(Sender: TObject);
begin
  e7zPath.Text := ExtractRelativepath(GetCurrentDir, e7zPath.Text);
  emPlayerPath.Text := ExtractRelativepath(GetCurrentDir, emPlayerPath.Text);
end;

procedure TfrmConfigManager.bAceptarClick(Sender: TObject);
begin
  // TODO 1: Change config values and close...
end;

procedure TfrmConfigManager.SetConfig(AValue: cConfig);
begin
  FConfig := AValue;

  if AValue = nil then Exit;

  lConfigFile.Caption:=AValue.ConfigFile;

  // Paths
  e7zPath.Text:= AValue.z7Subfolder + AValue.z7CMExecutable;
  e7zGPath.Text:= AValue.z7Subfolder + AValue.z7GExecutable;
  emPlayerPath.Text:=AValue.mPlayerSubfolder + AValue.mPlayerExecutable;

  // Extensions
  mImageExt.Clear;
  mImageExt.Lines.AddStrings(AValue.ImageExtensions);
  mTextExt.Clear;
  mTextExt.Lines.AddStrings(AValue.TextExtensions);
  mCompExt.Clear;
  mCompExt.Lines.AddStrings(AValue.CompressedExtensions);

end;

initialization
  {$I fConfigManager.lrs}

end.

