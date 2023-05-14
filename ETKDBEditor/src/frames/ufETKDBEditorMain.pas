unit ufETKDBEditorMain;
{< Main frame of ETK DB Editor.

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
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, Menus, ActnList,
  ComCtrls, StdActns, ExtCtrls, Buttons, StdCtrls, LazFileUtils,
  // CHX units
  uCHXStrUtils,
  // CHX frames
  ufCHXFrame,
  // Emuteca Core units
  uEmutecaConst;

type

  { TfmETKDBEditor }

  TfmETKDBEditor = class(TfmCHXFrame)
    alMain: TActionList;
    actExit: TFileExit;
    actOpenFile: TFileOpen;
    actSaveFile: TFileSaveAs;
    bExit: TButton;
    bOpenFile: TButton;
    bSaveFile: TButton;
    chkFastEditMode: TCheckBox;
    ilMain: TImageList;
    mimmExit: TMenuItem;
    mimmSave: TMenuItem;
    mimmOpen: TMenuItem;
    mimmFile: TMenuItem;
    pButtons: TPanel;
    sbMain: TStatusBar;
    Separator1: TMenuItem;
    sgMain: TStringGrid;
    procedure actOpenFileAccept(Sender: TObject);
    procedure actOpenFileBeforeExecute(Sender: TObject);
    procedure actSaveFileAccept(Sender: TObject);
    procedure actSaveFileBeforeExecute(Sender: TObject);
    procedure chkFastEditModeChange(Sender: TObject);
  private
    FCurrFile: string;
    procedure SetCurrFile(AValue: string);

  protected

    procedure DoClearFrameData;
    procedure DoLoadFrameData;

  public

  constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published

    property CurrFile: string read FCurrFile write SetCurrFile;

  end;

implementation

{$R *.lfm}

{ TfmETKDBEditor }

procedure TfmETKDBEditor.actOpenFileAccept(Sender: TObject);
begin
  CurrFile := actOpenFile.Dialog.FileName;
end;

procedure TfmETKDBEditor.actOpenFileBeforeExecute(Sender: TObject);
begin
  if CurrFile <> '' then
  begin
    actOpenFile.Dialog.FileName := SysPath(CurrFile);
    actOpenFile.Dialog.InitialDir := ExtractFileDir(actOpenFile.Dialog.FileName);
  end;
end;

procedure TfmETKDBEditor.actSaveFileAccept(Sender: TObject);
begin
  // FCurrFile, so we don't load it's contents again
  FCurrFile := actSaveFile.Dialog.FileName;

  sgMain.SaveToCSVFile(CurrFile);
end;

procedure TfmETKDBEditor.actSaveFileBeforeExecute(Sender: TObject);
begin
  if CurrFile = '' then Exit;

  actSaveFile.Dialog.FileName := SysPath(CurrFile);
  actSaveFile.Dialog.InitialDir := ExtractFileDir(actSaveFile.Dialog.FileName);
end;

procedure TfmETKDBEditor.chkFastEditModeChange(Sender: TObject);
var
  Options: TGridOptions;
begin
  Options := sgMain.Options;

  if chkFastEditMode.Checked then
  begin
    Include(Options, goAlwaysShowEditor);
    Exclude(Options, goRangeSelect);
  end
  else
  begin
    Exclude(Options, goAlwaysShowEditor);
    Include(Options, goRangeSelect);
  end;

  sgMain.Options := Options;
end;

procedure TfmETKDBEditor.SetCurrFile(AValue: string);
begin
  // Reloading file and removing not saved changes
  // if FCurrFile = AValue then Exit;

  FCurrFile := AValue;

  LoadFrameData;
end;

procedure TfmETKDBEditor.DoClearFrameData;
begin
  sgMain.Clear;
end;

procedure TfmETKDBEditor.DoLoadFrameData;
begin
  ClearFrameData;

  sgMain.Enabled := FileExistsUTF8(CurrFile);

  if not Enabled then Exit;

  sgMain.LoadFromCSVFile(CurrFile);
  sgMain.FixedRows := 1;

  sbMain.SimpleText := Format('%0:d rows.', [sgMain.RowCount - 1]);
end;

constructor TfmETKDBEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnLoadFrameData := @DoLoadFrameData;
  OnClearFrameData := @DoClearFrameData;

  actOpenFile.Dialog.Filter := 'Emuteca DBs|' + krsFileMaskGroup + ';' +
    krsFileMaskSoft + '|All Files|*.*';
  actSaveFile.Dialog.Filter := actOpenFile.Dialog.Filter;

  Enabled := True;
end;

destructor TfmETKDBEditor.Destroy;
begin
  inherited Destroy;
end;

end.

