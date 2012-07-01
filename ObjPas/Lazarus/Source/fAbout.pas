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

{Unit of About form}
unit fAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, StdCtrls, LCLIntf, strutils,
  uConfig, uGameManager, uVersionSupport;

resourcestring
  rsFABCopyright = 'Copyright';
  rsFABUnderLicense = 'Under license %0:s';

const
  kFABCopyright = '(C) 2006-2012 Chixpy';
  kFABLicense = 'GNU GPL v3';

type

  { A simple form with About info }

  TfrmAbout = class(TForm)
    bAcept: TBitBtn;
    gbxCompressedFiles: TGroupBox;
    gbxImageFiles: TGroupBox;
    gbxLegal: TGroupBox;
    gbxTechnicalInfo: TGroupBox;
    lCompressedFiles: TLabel;
    lImageFiles: TLabel;
    lLegalInfo: TLabel;
    lTempFile: TLabel;
    lTempFolder: TLabel;
    lTitle: TLabel;
    lVersion: TLabel;
    lViewLicense: TLabel;
    pBottom: TPanel;
    procedure lViewLicenseClick(Sender: TObject);

  private
    FConfig: cConfig;
    FGameManager: cGameManager;
    procedure SetConfig(const AValue: cConfig);
    procedure SetGameManager(const AValue: cGameManager);

  public
    property Config: cConfig read FConfig write SetConfig;
    {< cConfig object where some infomation is taken from}
    property GameManager: cGameManager read FGameManager write SetGameManager;
    {< cGameManager object where some infomation is taken from}
  end;

var
  frmAbout: TfrmAbout;

implementation

{ TfrmAbout }

procedure TfrmAbout.lViewLicenseClick(Sender: TObject);
begin
  OpenURL('http://www.gnu.org/copyleft/gpl.html');
end;

procedure TfrmAbout.SetConfig(const AValue: cConfig);

  procedure Translate;
  begin
      Self.Caption := Application.Title + ': ' + Self.Caption;
  end;

begin
  if FConfig = AValue then
    exit;
  FConfig := AValue;

  Translate;

  lTitle.Caption := Application.Title;

  lLegalInfo.Caption := rsFABCopyright + kFABCopyright +
    LineEnding + LineEnding + format(rsFABUnderLicense, [kFABLicense]);

  lImageFiles.Caption := AnsiReplaceText(Config.ImageExtensions.CommaText, ',', ' ');;

  lVersion.Caption := GetFileVersion + sLineBreak + '(' + GetCompiledDate + ')';
end;

procedure TfrmAbout.SetGameManager(const AValue: cGameManager);
begin
  if FGameManager = AValue then
    exit;
  FGameManager := AValue;

  lTempFolder.Caption := lTempFolder.Caption + ' ' + GameManager.TempFolder;
  lTempFile.Caption := lTempFile.Caption + ' ' + GameManager.TempFile;
  lCompressedFiles.Caption :=
    AnsiReplaceText(GameManager.CompressedExt.CommaText, ',', ' ');
end;

initialization
  {$I fAbout.lrs}

end.

