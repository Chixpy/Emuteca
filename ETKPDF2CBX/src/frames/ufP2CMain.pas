unit ufP2CMain;

{< ETK PDF2CBX main frame unit.

  This file is part of ETK PDF2CBX.

  Copyright (C) 2022 Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, EditBtn, FileUtil, LazFileUtils, LazUTF8, LCLIntf, FPReadJPEG,
  // CHX units
  uCHXRscStr, uCHXStrUtils, uCHXExecute, uCHX7zWrapper,
  // CHX frames
  ufCHXFrame, ufCHXImgListPreview,
  // ETKPDF2CBX classes
  ucP2CConfig;

const
  kPNMFileMask = '*.pnm;*.pbm;*.pgm;*.ppm';

resourcestring
  rsPNMFiles = 'WARNING: There are PNM files that you may want to convert and then reload the images.';

  rsPDFtoPNGErr1 = 'PDFtoPNG: Error opening a PDF file.';
  rsPDFtoPNGErr2 = 'PDFtoPNG: Error opening an output file.';
  rsPDFtoPNGErr3 = 'PDFtoPNG: Error related to PDF permissions.';
  rsPDFtoPNGErr99 = 'PDFtoPNG: Unknown error.';
  rsPDFtoPNGErrX = 'PDFtoPNG: No handled error.';

  rsPDFImagesErr1 = 'PDFImages: Error opening a PDF file.';
  rsPDFImagesErr2 = 'PDFImages: Error opening an output file.';
  rsPDFImagesErr3 = 'PDFImages: Error related to PDF permissions.';
  rsPDFImagesErr99 = 'PDFImages: Unknown error.';
  rsPDFImagesErrX = 'PDFImages: No handled error.';

type

  { TfmP2CMain

    Main frame of ETKPDF2CBX.
  }

  TfmP2CMain = class(TfmCHXFrame)
    bMakeCBX : TButton;
    bExtractPages : TButton;
    bOpenOutputFolder : TButton;
    bReloadConfig : TButton;
    bReloadFiles : TButton;
    bSaveConfig : TButton;
    bTempFolder : TButton;
    chkDeletePDF : TCheckBox;
    eImgEditorExecutable : TFileNameEdit;
    eImgEditorParams : TEdit;
    eOwnerPassword : TEdit;
    ePDFImagesExecutable : TFileNameEdit;
    eUserPassword : TEdit;
    ePDFtoPNGExecutable : TFileNameEdit;
    e7zExecutable : TFileNameEdit;
    ePDFFile : TFileNameEdit;
    gbxFiles : TGroupBox;
    gbxImagesPreview : TGroupBox;
    gbxInput : TGroupBox;
    gbxOutput : TGroupBox;
    gbxPasswords : TGroupBox;
    l7zExecutable : TLabel;
    lbxFiles : TListBox;
    lImgEditorExecutable : TLabel;
    lImgEditorParams : TLabel;
    lOwnerPassword : TLabel;
    lParamsHelp : TLabel;
    lPDFFile : TLabel;
    lPDFtoPNGExecutable : TLabel;
    lPDFImagesExecutable : TLabel;
    lUserPassword : TLabel;
    mConsoleLog : TMemo;
    pConfigButtons : TPanel;
    pgcMain : TPageControl;
    pLeft : TPanel;
    rgbOutputFormat : TRadioGroup;
    rgbPDFExtractor : TRadioGroup;
    Splitter1 : TSplitter;
    pagConvert : TTabSheet;
    pagConfig : TTabSheet;
    StatusBar : TStatusBar;
    pagConsoleLog : TTabSheet;
    procedure bExtractPagesClick(Sender : TObject);
    procedure bMakeCBXClick(Sender : TObject);
    procedure bOpenOutputFolderClick(Sender : TObject);
    procedure bReloadFilesClick(Sender : TObject);
    procedure bSaveConfigClick(Sender : TObject);
    procedure bTempFolderClick(Sender : TObject);
    procedure e7zExecutableEditingDone(Sender : TObject);
    procedure ePDFFileChange(Sender : TObject);

  private
    FfmImgListPreview : TfmCHXImgListPreview;
    FImageList : TStringList;
    FImageMask : string;
    FP2CConfig : cP2CConfig;
    FPDFExtracted : boolean;
    FTempFolder : string;
    procedure SetImageMask(AValue : string);
    procedure SetP2CConfig(AValue : cP2CConfig);
    procedure SetPDFExtracted(AValue : boolean);
    procedure SetTempFolder(AValue : string);

  protected
    property ImageMask : string read FImageMask write SetImageMask;

    property ImageList : TStringList read FImageList;
    property TempFolder : string read FTempFolder write SetTempFolder;
    property PDFExtracted : boolean read FPDFExtracted write SetPDFExtracted;

    property fmImgListPreview : TfmCHXImgListPreview read FfmImgListPreview;

    procedure Set7zWrapper;

    function ClearTempFolder : boolean;
    procedure LoadImages;
    procedure PDFtoPNGExtractFiles(aPDFFile : string);
    procedure PDFImagesExtractFiles(aPDFFile : string);

    procedure PDFExtractFiles(aPDFFile : string);

    procedure PrintExtractLog(CMDLine : TStringList;
      const sOutput, sError : string);

  public
    property P2CConfig : cP2CConfig read FP2CConfig write SetP2CConfig;

    procedure LoadConfig;
    procedure SaveConfig;

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmP2CMain }

procedure TfmP2CMain.bSaveConfigClick(Sender : TObject);
begin
  SaveConfig;
end;

procedure TfmP2CMain.bTempFolderClick(Sender : TObject);
begin
  OpenDocument(TempFolder);
end;

procedure TfmP2CMain.e7zExecutableEditingDone(Sender : TObject);
begin
  Set7zWrapper;
end;

procedure TfmP2CMain.ePDFFileChange(Sender : TObject);
begin
  bExtractPages.Enabled := False;
  bMakeCBX.Enabled := False;
  PDFExtracted := False;
  fmImgListPreview.FileList := nil;
  ImageList.Clear;

  if not ClearTempFolder then Exit;

  case ePDFFile.DialogFiles.Count of
    0: ePDFFile.Text := '';
    1:
    begin
      bExtractPages.Enabled := True;
      bMakeCBX.Enabled := True;
    end;
    else
    begin
      ePDFFile.Text := 'Various files selected';
      bMakeCBX.Enabled := True;
    end;
  end;

  if ePDFFile.DialogFiles.Count > 0 then
    ePDFFile.InitialDir := ExtractFileDir(ePDFFile.DialogFiles[0]);
end;


procedure TfmP2CMain.PDFtoPNGExtractFiles(aPDFFile : string);
var
  Options : TStringList;
  sError, sOutput : string;
  outError : integer;
begin
  if not FileExistsUTF8(aPDFFile) then
    Exit;

  Options := TStringList.Create;
  try

    Options.Add('-r');
    Options.Add('300');
    //Options.Add('−verbose');

    if eOwnerPassword.Text <> '' then
    begin
      Options.Add('-opw');
      Options.Add(eOwnerPassword.Text);
    end;

    if eOwnerPassword.Text <> '' then
    begin
      Options.Add('-upw');
      Options.Add(eUserPassword.Text);
    end;

    Options.Add(SysPath(aPDFFile));
    Options.Add(SysPath(TempFolder) + 'Page');

    if not ExecuteCMDSL('', P2CConfig.PDFtoPNGExe, Options,
      sOutput, sError, outError) then
      raise EFileNotFoundException.CreateFmt(rsFileNotFound,
        [P2CConfig.PDFtoPNGExe]);

    case outError of
      0: ; // OK
      1: ShowMessage(rsPDFtoPNGErr1);
      2: ShowMessage(rsPDFtoPNGErr2);
      3: ShowMessage(rsPDFtoPNGErr3);
      99: ShowMessage(rsPDFtoPNGErr99);
      else
        ShowMessage(rsPDFtoPNGErrX);
    end;

    if outError = 0 then
      PDFExtracted := True;

    PrintExtractLog(Options, sOutput, sError);

  finally
    FreeAndNil(Options);
  end;
end;

procedure TfmP2CMain.PDFImagesExtractFiles(aPDFFile : string);
var
  Options : TStringList;
  sError, sOutput : string;
  outError : integer;
begin
  if not FileExistsUTF8(aPDFFile) then
    Exit;

  Options := TStringList.Create;
  try

    Options.Add('-j');
    //Options.Add('−verbose');

    if eOwnerPassword.Text <> '' then
    begin
      Options.Add('-opw');
      Options.Add(eOwnerPassword.Text);
    end;

    if eOwnerPassword.Text <> '' then
    begin
      Options.Add('-upw');
      Options.Add(eUserPassword.Text);
    end;

    Options.Add(SysPath(aPDFFile));
    Options.Add(SysPath(TempFolder) + 'Page');

    if not ExecuteCMDSL('', P2CConfig.PDFImagesExe, Options,
      sOutput, sError, outError) then
      raise EFileNotFoundException.CreateFmt(rsFileNotFound,
        [P2CConfig.PDFImagesExe]);

    case outError of
      0: ; // OK
      1: ShowMessage(rsPDFImagesErr1);
      2: ShowMessage(rsPDFImagesErr2);
      3: ShowMessage(rsPDFImagesErr3);
      99: ShowMessage(rsPDFImagesErr99);
      else
        ShowMessage(rsPDFImagesErrX);
    end;

    if outError = 0 then
      PDFExtracted := True;

    PrintExtractLog(Options, sOutput, sError);

  finally
    FreeAndNil(Options);
  end;
end;

procedure TfmP2CMain.PDFExtractFiles(aPDFFile : string);
begin
  if not FileExistsUTF8(aPDFFile) then Exit;
  case rgbPDFExtractor.ItemIndex of
    0: PDFImagesExtractFiles(aPDFFile);
    1: PDFtoPNGExtractFiles(aPDFFile);
    else
      // This must not happen...
      PDFImagesExtractFiles(aPDFFile);
  end;
end;

procedure TfmP2CMain.PrintExtractLog(CMDLine : TStringList;
  const sOutput, sError : string);
begin
  mConsoleLog.Clear;
  mConsoleLog.Lines.Add('COMMAND LINE');
  mConsoleLog.Lines.Add('------------');
  mConsoleLog.Lines.AddStrings(CMDLine, False);

  mConsoleLog.Lines.Add('');
  mConsoleLog.Lines.Add('OUTPUT');
  mConsoleLog.Lines.Add('------');
  mConsoleLog.Lines.Add(sOutput);

  mConsoleLog.Lines.Add('');
  mConsoleLog.Lines.Add('ERROR');
  mConsoleLog.Lines.Add('-----');
  mConsoleLog.Lines.Add(sError);
  mConsoleLog.Lines.Add('');
end;

procedure TfmP2CMain.bExtractPagesClick(Sender : TObject);
var
  aPDFFile : string;
begin
  if ePDFFile.DialogFiles.Count <> 1 then
  begin
    bExtractPages.Enabled := False;
    Exit;
  end;

  aPDFFile := ePDFFile.DialogFiles[0];

  if not ClearTempFolder then Exit;

  PDFExtracted := False;

  if not FileExistsUTF8(aPDFFile) then
  begin
    ShowMessageFmt(rsFileNotFound, [aPDFFile]);
    Exit;
  end;

  StatusBar.SimpleText := 'Please Wait. Extracting images from: ' + aPDFFile;
  Self.Repaint;


  PDFExtractFiles(aPDFFile);

  StatusBar.SimpleText := 'Finished. Extracted images from: ' + aPDFFile;
  Self.Repaint;

  if PDFExtracted then
    LoadImages;
end;

procedure TfmP2CMain.bMakeCBXClick(Sender : TObject);
var
  PDFFile : string;
  ZipFile : string;
  i, AnError : integer;
begin
  AnError := 0;

  i := 0;
  while i < ePDFFile.DialogFiles.Count do
  begin
    PDFFile := ePDFFile.DialogFiles[i];

    StatusBar.SimpleText := 'Please Wait. Creating CBX: ' + PDFFile;
    Self.Repaint;

    // If there is only 1 file its already extracted
    if not PDFExtracted then
      PDFExtractFiles(PDFFile);

    ZipFile := ExtractFileNameWithoutExt(PDFFile);

    case rgbOutputFormat.ItemIndex of
      0: begin
        ZipFile := ZipFile + '.cbz';
        // AnError > 1 -> Error
        AnError := w7zCompressFolder(ZipFile, TempFolder, False, True, 'zip');
      end;
      1: begin
        ZipFile := ZipFile + '.cb7';
        // AnError > 1 -> Error
        AnError := w7zCompressFolder(ZipFile, TempFolder, False, True, '7z');
      end;
      else
      begin
        // True = 1; False = 0
        // We want AnError > 1 on False and AnError < 1 on True
        AnError := -(CopyDirTree(TempFolder,
          IncludeTrailingPathDelimiter(ZipFile),
          [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]).ToInteger) + 2;
      end;
    end;
    Inc(i);

    if AnError > 1 then
      ShowMessage('Error while creating CBX.');

    ImageList.Clear;
    ClearTempFolder;
    PDFExtracted := False;

    if chkDeletePDF.Checked and (AnError < 2) then
      DeleteFileUTF8(PDFFile);

    StatusBar.SimpleText := 'Finished. Creating CBX: ' + ZipFile;
    Self.Repaint;
  end;

  if chkDeletePDF.Checked then
  begin
    ePDFFile.DialogFiles.Clear;
    ePDFFile.FileName := '';
    bExtractPages.Enabled := False;
  end;

  bMakeCBX.Enabled := False;
  fmImgListPreview.FileList := nil;
  lbxFiles.Clear;
  ImageList.Clear;
end;

procedure TfmP2CMain.bOpenOutputFolderClick(Sender : TObject);
begin
  if ePDFFile.DialogFiles.Count > 0 then
    OpenDocument(ExtractFileDir(ePDFFile.DialogFiles[0]))
  else if ePDFFile.FileName <> '' then
    OpenDocument(ExtractFileDir(ePDFFile.FileName));
end;

procedure TfmP2CMain.bReloadFilesClick(Sender : TObject);
begin
  if PDFExtracted then
    LoadImages;
end;

procedure TfmP2CMain.LoadImages;

var
  i: LongInt;
begin
  ImageList.Clear;
  fmImgListPreview.FileList := nil;
  StatusBar.SimpleText := '';

  // HACK: Warning the user that are some PNM files
  ImageList.Clear;
  FindAllFiles(ImageList, TempFolder, kPNMFileMask);
  if ImageList.Count > 0 then
    StatusBar.SimpleText := rsPNMFiles;

  // Listing all files
  ImageList.Clear;
  lbxFiles.Clear;
  FindAllFiles(ImageList, TempFolder, AllFilesMask);
  i:=0;
  while i < ImageList.Count do
  begin
    lbxFiles.Items.Add(SetAsRelativeFile(ImageList[i], TempFolder));
    Inc(i);
  end;

  ImageList.Clear;
  FindAllFiles(ImageList, TempFolder, ImageMask);
  fmImgListPreview.FileList := ImageList;
end;

procedure TfmP2CMain.SetP2CConfig(AValue : cP2CConfig);
begin
  if FP2CConfig = AValue then Exit;
  FP2CConfig := AValue;

  LoadFrameData;
end;

procedure TfmP2CMain.SetImageMask(AValue : string);
begin
  if FImageMask = AValue then Exit;
  FImageMask := AValue;
end;

procedure TfmP2CMain.SetPDFExtracted(AValue : boolean);
begin
  FPDFExtracted := AValue;

  bReloadFiles.Enabled := PDFExtracted;
  bMakeCBX.Enabled := PDFExtracted;
end;

procedure TfmP2CMain.SetTempFolder(AValue : string);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure TfmP2CMain.Set7zWrapper;
var
  aPath : string;
begin
  w7zSetPathTo7zexe(e7zExecutable.Text);

  // Well try 7zG.exe too
  aPath := SetAsFolder(ExtractFileDir(e7zExecutable.Text)) + '7zG.exe';
  if FileExistsUTF8(aPath) then
    w7zSetPathTo7zGexe(aPath)
  else
    w7zSetPathTo7zGexe('');
end;

function TfmP2CMain.ClearTempFolder : boolean;
begin
  Result := False;

  if DirectoryExistsUTF8(TempFolder) then
    if not DeleteDirectory(UTF8ToSys(TempFolder), False) then
    begin
      ShowMessageFmt(rsErrorDeletingFile, [TempFolder]);
      Exit;
    end;
  if not ForceDirectoriesUTF8(TempFolder) then
  begin
    ShowMessageFmt(rsErrorCreatingFile, [TempFolder]);
    Exit;
  end;

  Result := True;
end;

procedure TfmP2CMain.LoadConfig;
begin
  if not assigned(P2CConfig) then Exit;

  ePDFtoPNGExecutable.Text := P2CConfig.PDFtoPNGExe;
  ePDFImagesExecutable.Text := P2CConfig.PDFImagesExe;
  e7zExecutable.Text := P2CConfig.SevenZipExe;
  Set7zWrapper;
  eImgEditorExecutable.Text := P2CConfig.ImgEditorExe;
  eImgEditorParams.Text := P2CConfig.ImgEditorParams;

  ePDFFile.InitialDir := ExcludeTrailingPathDelimiter(
    SysPath(P2CConfig.LastFolder));
end;

procedure TfmP2CMain.SaveConfig;
begin
  if not assigned(P2CConfig) then Exit;

  P2CConfig.PDFtoPNGExe := ePDFtoPNGExecutable.Text;
  P2CConfig.PDFImagesExe := ePDFImagesExecutable.Text;
  P2CConfig.SevenZipExe := e7zExecutable.Text;
  P2CConfig.ImgEditorExe := eImgEditorExecutable.Text;
  P2CConfig.ImgEditorParams := eImgEditorParams.Text;
  P2CConfig.LastFolder := ePDFFile.InitialDir;
end;

procedure TfmP2CMain.ClearFrameData;
begin
  ePDFtoPNGExecutable.Text := '';
  ePDFImagesExecutable.Text := '';
  e7zExecutable.Text := '';
  Set7zWrapper;
  eImgEditorExecutable.Text := '';
  eImgEditorParams.Text := '';
end;

procedure TfmP2CMain.LoadFrameData;
begin
  Enabled := assigned(P2CConfig);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  LoadConfig;
end;

constructor TfmP2CMain.Create(TheOwner : TComponent);
var
  i : integer;
begin
  inherited Create(TheOwner);

  FfmImgListPreview := TfmCHXImgListPreview.Create(gbxImagesPreview);
  fmImgListPreview.Align := alClient;
  fmImgListPreview.Parent := gbxImagesPreview;

  TempFolder := IncludeTrailingPathDelimiter(GetTempDir) + 'ETKPDF2CBX';
  FImageList := TStringList.Create;

  pgcMain.ActivePage := pagConvert;

  // HACK: Removing PNM files, PNM support of binary files is broken in FP...
  ImageMask := GraphicFileMask(TGraphic);
  ImageMask := UTF8TextReplace(ImageMask, '*.pnm', '');
  ImageMask := UTF8TextReplace(ImageMask, '*.pbm', '');
  ImageMask := UTF8TextReplace(ImageMask, '*.pgm', '');
  ImageMask := UTF8TextReplace(ImageMask, '*.ppm', '');
  i := 1;
  while i > 0 do
    ImageMask := UTF8StringReplace(ImageMask, ';;', ';',
      [rfReplaceAll, rfIgnoreCase], i);
end;

destructor TfmP2CMain.Destroy;
begin
  FreeAndNil(FImageList);
  DeleteDirectory(UTF8ToSys(TempFolder), False);

  inherited Destroy;
end;

end.
{
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
