unit ufETKDBEditorMain;

{< Main frame of ETK DB Editor.

  This file is part of Emuteca.

  Copyright (C) 2023 Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids,
  Menus, ActnList, LCLType, Clipbrd, LazUTF8,
  ComCtrls, StdActns, ExtCtrls, Buttons, StdCtrls, LazFileUtils,
  // CHX units
  uCHXRscStr, uCHXStrUtils,
  // CHX frames
  ufCHXFrame,
  // Emuteca Core units
  uEmutecaConst, uEmutecaRscStr,
  // ETKDBEditor units
  uETKDBERscStr;

type

  { TfmETKDBEditor }

  TfmETKDBEditor = class(TfmCHXFrame)
    alMain : TActionList;
    actExit : TFileExit;
    actOpenFile : TFileOpen;
    actSaveFile : TFileSaveAs;
    bExit : TButton;
    bOpenFile : TButton;
    bSaveFile : TButton;
    cbxFastMove : TComboBox;
    chkFastEditMode : TCheckBox;
    ilMain : TImageList;
    lFastMove : TLabel;
    mimmExit : TMenuItem;
    mimmSave : TMenuItem;
    mimmOpen : TMenuItem;
    mimmFile : TMenuItem;
    pButtons : TPanel;
    pFastMove : TPanel;
    sbMain : TStatusBar;
    Separator1 : TMenuItem;
    sgMain : TStringGrid;
    procedure actOpenFileAccept(Sender : TObject);
    procedure actOpenFileBeforeExecute(Sender : TObject);
    procedure actSaveFileAccept(Sender : TObject);
    procedure actSaveFileBeforeExecute(Sender : TObject);
    procedure chkFastEditModeChange(Sender : TObject);
    procedure sgMainGetCellHint(Sender : TObject; ACol, ARow : integer;
      var HintText : string);
    procedure sgMainKeyDown(Sender : TObject; var Key : word;
      Shift : TShiftState);
    procedure sgMainSelectCell(Sender : TObject; aCol, aRow : integer;
      var CanSelect : boolean);
    procedure sgMainValidateEntry(Sender : TObject; aCol, aRow : integer;
      const OldValue : string; var NewValue : string);
  private
    FCurrFile : string;
    procedure SetCurrFile(AValue : string);

  protected
    procedure PasteToSelection;
    { Pastes current Clipboard content in each selected cells. Removes selected
        cells contents.

      Line endings and tabs (which are cell separators if many cells are
        copied) are replaced by Emuteca's value separators " | ".

      Only works in NO Fast edit mode (you only can select multiple cells in
        this mode).
    }
    procedure FastMove;
    { Cuts and moves current selection (cell or text) to the column defined in
        cbxFastMove in the same row.

      Multiple selected cells in the same row are separated by " | " in target
        column. Multiple rows can be selected too.

      In edit mode, current selected text is cut and added to the target column.
    }

    function FormatCellText(aText : string) : string;

  public
    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;

  published
    property CurrFile : string read FCurrFile write SetCurrFile;

  end;

implementation

{$R *.lfm}

{ TfmETKDBEditor }

procedure TfmETKDBEditor.actOpenFileAccept(Sender : TObject);
begin
  CurrFile := actOpenFile.Dialog.FileName;
end;

procedure TfmETKDBEditor.actOpenFileBeforeExecute(Sender : TObject);
begin
  if not CurrFile.IsEmpty then
  begin
    actOpenFile.Dialog.FileName := SysPath(CurrFile);
    actOpenFile.Dialog.InitialDir :=
      ExtractFileDir(actOpenFile.Dialog.FileName);
  end;
end;

procedure TfmETKDBEditor.actSaveFileAccept(Sender : TObject);
begin
  // FCurrFile, so we don't load it's contents again
  FCurrFile := actSaveFile.Dialog.FileName;

  sgMain.SaveToCSVFile(CurrFile);

  sgMain.Modified := False;
end;

procedure TfmETKDBEditor.actSaveFileBeforeExecute(Sender : TObject);
begin
  if CurrFile.IsEmpty then
    Exit;

  actSaveFile.Dialog.FileName := SysPath(CurrFile);
  actSaveFile.Dialog.InitialDir := ExtractFileDir(actSaveFile.Dialog.FileName);
end;

procedure TfmETKDBEditor.chkFastEditModeChange(Sender : TObject);
var
  Options : TGridOptions;
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

procedure TfmETKDBEditor.sgMainGetCellHint(Sender : TObject;
  ACol, ARow : integer; var HintText : string);
begin
  HintText := sgMain.Cells[ACol, ARow];
end;

procedure TfmETKDBEditor.sgMainKeyDown(Sender : TObject;
  var Key : word; Shift : TShiftState);
begin
  case Key of

    VK_DELETE: begin
      if chkFastEditMode.Checked then
        Exit;

      sgMain.Clean(sgMain.Selection, []);
      // Don't process further
      Key := 0;
    end;

    VK_A: begin
      // CTRL+A: Paste Clipboard in all selected cells
      if (Key <> 0) and (ssModifier in Shift) then
      begin
        PasteToSelection;
        // Don't process further
        Key := 0;
      end;

      // ALT+A: CUT & PASTE current selection in desired column.
      if (Key <> 0) and (ssAlt in Shift) then
      begin
        FastMove;
        // Don't process further
        Key := 0;
      end;
    end;
  end;
end;

procedure TfmETKDBEditor.sgMainSelectCell(Sender : TObject;
  aCol, aRow : integer; var CanSelect : boolean);
begin
  sbMain.Panels[0].Text :=
    Format(rsCellInfoFmt, [aCol, aRow, sgMain.RowCount]);

  CanSelect := True;
end;

procedure TfmETKDBEditor.sgMainValidateEntry(Sender : TObject;
  aCol, aRow : integer; const OldValue : string; var NewValue : string);
begin
  NewValue := FormatCellText(NewValue);
end;

procedure TfmETKDBEditor.SetCurrFile(AValue : string);
begin
  // Reloading file and removing not saved changes
  // if FCurrFile = AValue then Exit;

  FCurrFile := AValue;

  // Enable if a file is loaded.
  actSaveFile.Enabled := FileExistsUTF8(CurrFile);

  LoadFrameData;
end;

procedure TfmETKDBEditor.PasteToSelection;
var
  i, j : LongInt;
  aText : string;
begin
  if chkFastEditMode.Checked then
    Exit;

  // Replacing Tabs and Line Endings
  aText := Clipboard.AsText;
  aText := UTF8TextReplace(aText, #9, krsValueSeparator);
  aText := UTF8TextReplace(aText, #13#10, krsValueSeparator);
  aText := UTF8TextReplace(aText, #13, krsValueSeparator);
  aText := UTF8TextReplace(aText, #10, krsValueSeparator);

  i := sgMain.Selection.Top;
  while i <= sgMain.Selection.Bottom do
  begin
    j := sgMain.Selection.Left;
    while j <= sgMain.Selection.Right do
    begin
      sgMain.Cells[j, i] := aText;
      Inc(j);
    end;
    Inc(i);
  end;
end;

procedure TfmETKDBEditor.FastMove;
var
  i, j : LongInt;
  aText : string;
begin
  if sgMain.EditorMode then
  begin
    // We are editing a cell. Cut selected text an paste
    if sgMain.Editor is TCustomEdit then
      TCustomEdit(sgMain.Editor).CutToClipboard
    else
      Exit;

    if sgMain.Cells[cbxFastMove.ItemIndex, sgMain.Row].IsEmpty then
      sgMain.Cells[cbxFastMove.ItemIndex, sgMain.Row] :=
        FormatCellText(Clipboard.AsText)
    else
      sgMain.Cells[cbxFastMove.ItemIndex, sgMain.Row] :=
        FormatCellText(sgMain.Cells[cbxFastMove.ItemIndex, sgMain.Row] +
        krsValueSeparator + Clipboard.AsText);
  end
  else
  begin
    // Moving selected cells to the cbxFastMove column

    i := sgMain.Selection.Top;
    while i <= sgMain.Selection.Bottom do
    begin
      aText := sgMain.Cells[cbxFastMove.ItemIndex, i];

      j := sgMain.Selection.Left;
      while j <= sgMain.Selection.Right do
      begin
        if not sgMain.Cells[j, i].IsEmpty then
        begin
          if aText.IsEmpty then
            aText := sgMain.Cells[j, i]
          else
            aText := aText + krsValueSeparator + sgMain.Cells[j, i];

          sgMain.Cells[j, i] := EmptyStr;
        end;

        Inc(j);
      end;

      sgMain.Cells[cbxFastMove.ItemIndex, i] := FormatCellText(aText);

      Inc(i);
    end;
  end;
end;

function TfmETKDBEditor.FormatCellText(aText : string) : string;
begin
  Result := aText;

  // Removing double spaces
  while UTF8Pos('  ', Result) > 0 do
    Result := UTF8TextReplace(Result, '  ', ' ');

  // Removing double separators
  while UTF8Pos('| |', Result) > 0 do
    Result := UTF8TextReplace(Result, '| |', '|');
  while UTF8Pos('||', Result) > 0 do
    Result := UTF8TextReplace(Result, '||', '|');

  // Removing spaces and separators at
  while (not Result.IsEmpty) and ((Result[1] = ' ') or (Result[1] = '|')) do
    Result := Copy(Result, 2, Length(Result));
  while (not Result.IsEmpty) and ((Result[Length(Result)] = ' ') or
      (Result[Length(Result)] = '|')) do
    Result := Copy(Result, 1, Length(Result) - 1);
end;

procedure TfmETKDBEditor.ClearFrameData;
begin
  inherited ClearFrameData;

  sgMain.Clear;
  sbMain.Panels[0].Text := EmptyStr;
end;

procedure TfmETKDBEditor.LoadFrameData;
var
  i : LongInt;
begin
  inherited LoadFrameData;

  sgMain.Enabled := FileExistsUTF8(CurrFile);

  if not sgMain.Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  sgMain.LoadFromCSVFile(CurrFile);

  cbxFastMove.Clear;

  if sgMain.RowCount > 0 then
  begin
    sgMain.FixedRows := 1;

    if sgMain.ColCount > 0 then
      sgMain.FixedCols := 1;

    i := 0;
    while i < sgMain.ColCount do
    begin
      cbxFastMove.AddItem(sgMain.Cells[i, 0], nil);
      Inc(i);
    end;
    cbxFastMove.ItemIndex := 0;
  end;

  sbMain.Panels[0].Text := Format(rsCellInfoFmt, [0, 0, sgMain.RowCount]);

  sgMain.Modified := False;
end;

constructor TfmETKDBEditor.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);

  actOpenFile.Dialog.Filter :=
    rsFileMaskDescDB + '|' + krsFileMaskGroup + ';' + krsFileMaskSoft +
    '|' + rsFileDlgMaskDef;
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
{ This source is free software; you can redistribute it and/or modify it under
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
