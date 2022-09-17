unit ufrMain;

{< ETK PDF2CBX main form unit.

  This file is part of ETK PDF2CBX.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, FileUtil,
  LazFileUtils, LCLTranslator,
  // Misc units
  uVersionSupport,
  // CHX units
  uCHXConst, uCHXStrUtils,
  // CHX forms
  ufrCHXForm,
  // ETKPDF2CBX units
  uP2CConst,
  // ETKPDF2CBX classes
  ucP2CConfig,
  // ETKPDF2CBX frames
  ufP2CMain;

type

  { TfrMain }

  TfrMain = class(TfrmCHXForm)
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);

  private
    FBaseFolder: string;
    FfmMain: TfmP2CMain;
    FP2CConfig: cP2CConfig;
    procedure SetBaseFolder(AValue: string);
    procedure SetP2CConfig(AValue: cP2CConfig);

  protected
    property fmMain: TfmP2CMain read FfmMain;

    property P2CConfig: cP2CConfig read FP2CConfig write SetP2CConfig;

  public

  published
    property BaseFolder: string read FBaseFolder write SetBaseFolder;
  end;

var
  frMain: TfrMain;

implementation

{$R *.lfm}

{ TfrMain }

procedure TfrMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;

  fmMain.SaveConfig;

  P2CConfig.SaveToFile('', False);
  FreeAndNil(FP2CConfig);
end;

procedure TfrMain.FormCreate(Sender: TObject);

  procedure CreateFrames;
  begin
    FfmMain := TfmP2CMain.Create(self);
    fmMain.Align := alClient;
    fmMain.Parent := self;
    fmMain.P2CConfig := P2CConfig;
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
  FP2CConfig := cP2CConfig.Create(self);
  P2CConfig.DefaultFileName :=
    SetAsAbsoluteFile(krsP2CName + '.ini', BaseFolder);

  LoadGUIConfig(P2CConfig.DefaultFileName);
  P2CConfig.LoadFromFile('');

  CreateFrames;
end;

procedure TfrMain.SetBaseFolder(AValue: string);
begin
  FBaseFolder := SetAsFolder(AValue);
end;

procedure TfrMain.SetP2CConfig(AValue: cP2CConfig);
begin
  if FP2CConfig = AValue then Exit;
  FP2CConfig := AValue;
end;

end.
