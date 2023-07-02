unit ufrETKDBEditorMain;

{< Main form of ETK DB Editor.

  This file is part of Emuteca.

  Copyright (C) 2023 Chixpy

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
  uCHXConst, uCHXRscStr, uCHXStrUtils,
  // CHX forms
  ufrCHXForm,
  // DB Editor units
  uETKDBEConst,
  // DB Editor frames
  ufETKDBEditorMain;

type

  { TfrmETKDBEditorMain }

  TfrmETKDBEditorMain = class(TfrmCHXForm)
    procedure FormCreate(Sender: TObject);

  private
    FBaseFolder: string;
    FfmETKDBEditor: TfmETKDBEditor;
    procedure SetBaseFolder(AValue: string);

  protected
    property fmETKDBEditor: TfmETKDBEditor read FfmETKDBEditor;

    property BaseFolder: string read FBaseFolder write SetBaseFolder;

  public


  end;

var
  frmETKDBEditorMain: TfrmETKDBEditorMain;

implementation

{$R *.lfm}

{ TfrmETKDBEditorMain }

procedure TfrmETKDBEditorMain.FormCreate(Sender: TObject);

  procedure CreateFrames;
  begin
    FfmETKDBEditor := TfmETKDBEditor.Create(self);
    fmETKDBEditor.Align := alClient;
    fmETKDBEditor.Parent := self;
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

  CreateFrames;

  // Loading GUI config
  // FEMCConfig := cEMCConfig.Create(self);
  // EMCConfig.DefaultFileName := SetAsAbsoluteFile(krsEMCName + '.ini', BaseFolder);

  LoadGUIConfig(SetAsAbsoluteFile(krsDBEName + '.ini', BaseFolder));
  // EMCConfig.LoadFromFile('');



  // Reading commandline parameters
  if ParamCount > 0 then
  begin
    // First parameter: File to open
    if ParamStr(1) <> '' then
    begin
      if not FileExistsUTF8(ParamStr(1)) then
        ShowMessageFmt(rsFileNotFound, [ParamStr(1)])
      else
        fmETKDBEditor.CurrFile := ParamStr(1);
    end;
  end;
end;

procedure TfrmETKDBEditorMain.SetBaseFolder(AValue: string);
begin
  FBaseFolder := SetAsFolder(AValue);
end;

end.
