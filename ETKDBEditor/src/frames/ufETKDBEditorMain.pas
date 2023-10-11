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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids,
  Menus, ActnList, LCLType, Clipbrd,
  ComCtrls, StdActns, ExtCtrls, Buttons, StdCtrls, LazFileUtils,
  // CHX units
  uCHXRscStr, uCHXStrUtils,
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
    procedure sgMainGetCellHint(Sender: TObject; ACol, ARow: integer;
      var HintText: string);
    procedure sgMainKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
  private
    FCurrFile: string;
    procedure SetCurrFile(AValue: string);

  protected

  public
    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

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
    actOpenFile.Dialog.InitialDir :=
      ExtractFileDir(actOpenFile.Dialog.FileName);
  end;
end;

procedure TfmETKDBEditor.actSaveFileAccept(Sender: TObject);
begin
  // FCurrFile, so we don't load it's contents again
  FCurrFile := actSaveFile.Dialog.FileName;

  sgMain.SaveToCSVFile(CurrFile);

  sgMain.Modified := False;
end;

procedure TfmETKDBEditor.actSaveFileBeforeExecute(Sender: TObject);
begin
  if CurrFile = '' then
    Exit;

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

procedure TfmETKDBEditor.sgMainGetCellHint(Sender: TObject;
  ACol, ARow: integer; var HintText: string);
begin
  HintText := sgMain.Cells[ACol, ARow];
end;

procedure TfmETKDBEditor.sgMainKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
var
  i, j: LongInt;
begin

  if chkFastEditMode.Checked then
    Exit;

  case Key of
    VK_DELETE: begin
      sgMain.Clean(sgMain.Selection, []);
      // Don't process further
      Key := 0;
    end;
    VK_A: begin
      if ssModifier in Shift then
      begin
        i := sgMain.Selection.Top;
        while i <= sgMain.Selection.Bottom do
        begin
          j := sgMain.Selection.Left;
          while j <= sgMain.Selection.Right do
          begin
            sgMain.Cells[j, i] := Clipboard.AsText;

            // Removing line endings
            sgMain.Cells[j, i] :=
              UTF8TextReplace(sgMain.Cells[j, i], #13#10, ' ');
            sgMain.Cells[j, i] :=
              UTF8TextReplace(sgMain.Cells[j, i], #13, ' ');
            sgMain.Cells[j, i] :=
              UTF8TextReplace(sgMain.Cells[j, i], #10, ' ');
            sgMain.Cells[j, i] := Trim(sgMain.Cells[j, i]);

            Inc(j);
          end;
          Inc(i);
        end;
        // Don't process further
        Key := 0;
      end;
    end;
  end;
end;

procedure TfmETKDBEditor.SetCurrFile(AValue: string);
begin
  // Reloading file and removing not saved changes
  // if FCurrFile = AValue then Exit;

  FCurrFile := AValue;

  // Enable if a file is loaded.
  actSaveFile.Enabled := FileExistsUTF8(CurrFile);

  LoadFrameData;
end;

procedure TfmETKDBEditor.ClearFrameData;
begin
  inherited ClearFrameData;

  sgMain.Clear;
  sbMain.SimpleText := '';
end;

procedure TfmETKDBEditor.LoadFrameData;
begin
  inherited LoadFrameData;

  sgMain.Enabled := FileExistsUTF8(CurrFile);

  if not sgMain.Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  sgMain.LoadFromCSVFile(CurrFile);
  if sgMain.RowCount > 0 then
    sgMain.FixedRows := 1; // Empty files error

  sbMain.SimpleText := Format('%0:d rows.', [sgMain.RowCount - 1]);

  sgMain.Modified := False;
end;

constructor TfmETKDBEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  actOpenFile.Dialog.Filter :=
    'Emuteca DBs|' + krsFileMaskGroup + ';' + krsFileMaskSoft +
    '|All Files|*.*';
  actSaveFile.Dialog.Filter := actOpenFile.Dialog.Filter;

  Enabled := True;
end;

destructor TfmETKDBEditor.Destroy;
begin
  if sgMain.Modified and FileExistsUTF8(CurrFile) and
    (mrYes = MessageDlg(rsSaveChangesCaption, rsSaveChanges,
    mtConfirmation, [mbYes, mbNo], '')) then
    sgMain.SaveToCSVFile(CurrFile);

  inherited Destroy;
end;

end.
