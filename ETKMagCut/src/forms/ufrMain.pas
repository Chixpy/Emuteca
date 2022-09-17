unit ufrMain;
{< ETK Magazine Cutter main form.

  This file is part of ETK Magazine.

  Copyright (C) 2022 Chixpy

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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, FileUtil, LazFileUtils, LCLTranslator,
  // Miscelaneous units
  uVersionSupport,
  // CHX units
  uCHXStrUtils, uCHXConst,
  // CHX forms
  ufrCHXForm,
  // CHX frames
  ufCHXFileList,
  // EMC units
  uEMCConst,
  // EMC classes
  ucEMCConfig,
  // ETK Magazine Cutter frames
  ufEMCMain;

type

  { TfrmETKMagazineCutter }

  TfrmETKMagazineCutter = class(TfrmCHXForm)
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);

  private
    FBaseFolder: string;
    FfmMain: TfmEMCMain;
    FEMCConfig: cEMCConfig;
    procedure SetBaseFolder(AValue: string);
    procedure SetEMCConfig(AValue: cEMCConfig);

  protected
    property fmMain: TfmEMCMain read FfmMain;

    property EMCConfig: cEMCConfig read FEMCConfig write SetEMCConfig;

  public

  published
    property BaseFolder: string read FBaseFolder write SetBaseFolder;

  end;

var
  frmETKMagazineCutter: TfrmETKMagazineCutter;

implementation

{$R *.lfm}

{ TfrmETKMagazineCutter }

procedure TfrmETKMagazineCutter.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := True;

  fmMain.SaveEMCConfig;
  EMCConfig.SaveToFile('', False);
  FreeAndNil(FEMCConfig);
end;

procedure TfrmETKMagazineCutter.FormCreate(Sender: TObject);

  procedure CreateFrames;
  begin
    FfmMain := TfmEMCMain.Create(self);
    fmMain.Align := alClient;
    fmMain.Parent := self;
    fmMain.EMCConfig := EMCConfig;
  end;

var
  TempStr: string;
begin
  // Title of application, usually it's autodeleted in .lpr file...
  Application.Title := Format(krsFmtApplicationTitle,
    [Application.Title, GetFileVersion]);

  // Changing base folder to parents exe folder.
  BaseFolder := ExtractFileDir(ExcludeTrailingPathDelimiter(
    ProgramDirectory));
  ChDir(BaseFolder);

  // Loading translation
  TempStr := BaseFolder + krsLocaleFolder;
  if not DirectoryExistsUTF8(TempStr) then
    mkdir(TempStr);
  SetDefaultLang('', TempStr);

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings which can cause errors.
  StandardFormatSettings;

  // Windows Caption
  Caption := Format(krsFmtWindowCaption, [Application.Title, Caption]);

  // Loading GUI config
  FEMCConfig := cEMCConfig.Create(self);
  EMCConfig.DefaultFileName := SetAsAbsoluteFile(krsEMCName + '.ini', BaseFolder);

  LoadGUIConfig(EMCConfig.DefaultFileName);
  EMCConfig.LoadFromFile('');

  CreateFrames;
end;

procedure TfrmETKMagazineCutter.SetBaseFolder(AValue: string);
begin
  FBaseFolder := SetAsFolder(AValue);
end;

procedure TfrmETKMagazineCutter.SetEMCConfig(AValue: cEMCConfig);
begin
  if FEMCConfig = AValue then Exit;
  FEMCConfig := AValue;
end;

end.
