unit ufP2CMain;

{< ETK PDF2CBX main frame unit.

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
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, EditBtn, FileUtil, LazFileUtils, LazUTF8, LCLIntf, FPReadJPEG,
  // CHX units
  uCHXRscStr, uCHXStrUtils, uCHXExecute, uCHX7zWrapper, uCHXDlgUtils,
  // CHX frames
  ufCHXFrame, ufCHXImgListPreview,
  // ETKPDF2CBX classes
  ucP2CConfig;

type

  { TfmP2CMain }

  TfmP2CMain = class(TfmCHXFrame)
    bMakeCBX: TButton;
    bExtractPages: TButton;
    bOpenOutputFolder: TButton;
    bReloadConfig: TButton;
    bReloadImages: TButton;
    bSaveConfig: TButton;
    bTempFolder: TButton;
    chkConvert2JPG: TCheckBox;
    chkDeletePDF: TCheckBox;
    chkResize2048: TCheckBox;
    eImgEditorExecutable: TFileNameEdit;
    eImgEditorParams: TEdit;
    eOwnerPassword: TEdit;
    ePDFImagesExecutable: TFileNameEdit;
    eUserPassword: TEdit;
    ePDFtoPNGExecutable: TFileNameEdit;
    e7zExecutable: TFileNameEdit;
    ePDFFile: TFileNameEdit;
    gbxImagesPreview: TGroupBox;
    gbxInput: TGroupBox;
    gbxOutput: TGroupBox;
    gbxPasswords: TGroupBox;
    l7zExecutable: TLabel;
    lImgEditorExecutable: TLabel;
    lImgEditorParams: TLabel;
    lOwnerPassword: TLabel;
    lParamsHelp: TLabel;
    lPDFFile: TLabel;
    lPDFtoPNGExecutable: TLabel;
    lPDFImagesExecutable: TLabel;
    lUserPassword: TLabel;
    mConsoleLog: TMemo;
    pConfigButtons: TPanel;
    pgcMain: TPageControl;
    pLeft: TPanel;
    rgbOutputFormat: TRadioGroup;
    rgbPDFExtractor: TRadioGroup;
    Splitter1: TSplitter;
    pagConvert: TTabSheet;
    pagConfig: TTabSheet;
    StatusBar: TStatusBar;
    pagConsoleLog: TTabSheet;
    procedure bExtractPagesClick(Sender: TObject);
    procedure bMakeCBXClick(Sender: TObject);
    procedure bOpenOutputFolderClick(Sender: TObject);
    procedure bReloadImagesClick(Sender: TObject);
    procedure bSaveConfigClick(Sender: TObject);
    procedure bTempFolderClick(Sender: TObject);
    procedure e7zExecutableEditingDone(Sender: TObject);
    procedure ePDFFileChange(Sender: TObject);

  private
    FfmImgListPreview: TfmCHXImgListPreview;
    FImageList: TStringList;
    FImageMask: string;
    FP2CConfig: cP2CConfig;
    FPDFExtracted: boolean;
    FTempFolder: string;
    procedure SetImageMask(AValue: string);
    procedure SetP2CConfig(AValue: cP2CConfig);
    procedure SetPDFExtracted(AValue: boolean);
    procedure SetTempFolder(AValue: string);

  protected
    property ImageMask: string read FImageMask write SetImageMask;

    property ImageList: TStringList read FImageList;
    property TempFolder: string read FTempFolder write SetTempFolder;
    property PDFExtracted: boolean read FPDFExtracted write SetPDFExtracted;

    property fmImgListPreview: TfmCHXImgListPreview read FfmImgListPreview;

    procedure Set7zWrapper;

    function ClearTempFolder: boolean;
    procedure LoadImages;
    procedure PDFtoPNGExtractFiles(aPDFFile: string);
    procedure PDFImagesExtractFiles(aPDFFile: string);

    procedure PDFExtractFiles(aPDFFile: string);

    procedure Convert2JPG;

    procedure PrintExtractLog(CMDLine: TStringList;
      msOutput, msError: TMemoryStream);

    procedure LoadConfig;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;


  public
    property P2CConfig: cP2CConfig read FP2CConfig write SetP2CConfig;

    procedure SaveConfig;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmP2CMain }

procedure TfmP2CMain.bSaveConfigClick(Sender: TObject);
begin
  SaveConfig;
end;

procedure TfmP2CMain.bTempFolderClick(Sender: TObject);
begin
  OpenDocument(TempFolder);
end;

procedure TfmP2CMain.e7zExecutableEditingDone(Sender: TObject);
begin
  Set7zWrapper;
end;

procedure TfmP2CMain.ePDFFileChange(Sender: TObject);
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


procedure TfmP2CMain.PDFtoPNGExtractFiles(aPDFFile: string);
var
  Options: TStringList;
  msError, msOutput: TMemoryStream;
  outError: integer;
begin
  if not FileExistsUTF8(aPDFFile) then
    Exit;

  Options := TStringList.Create;
  msError := TMemoryStream.Create;
  msOutput := TMemoryStream.Create;
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
      msOutput, msError, outError) then
      raise EFileNotFoundException.CreateFmt(rsFileNotFound,
        [P2CConfig.PDFtoPNGExe]);

    case outError of
      0: ; // OK
      1: ShowMessage('PDFtoPNG: Error opening a PDF file.');
      2: ShowMessage('PDFtoPNG: Error opening an output file.');
      3: ShowMessage('PDFtoPNG: Error related to PDF permissions.');
      99: ShowMessage('PDFtoPNG: Unknown error.');
      else
        ShowMessage('PDFtoPNG: No handled error.');
    end;

    if outError = 0 then
      PDFExtracted := True;

    PrintExtractLog(Options, msOutput, msError);

  finally
    FreeAndNil(Options);
    FreeAndNil(msError);
    FreeAndNil(msOutput);
  end;
end;

procedure TfmP2CMain.PDFImagesExtractFiles(aPDFFile: string);
var
  Options: TStringList;
  msError, msOutput: TMemoryStream;
  outError: integer;
begin
  if not FileExistsUTF8(aPDFFile) then
    Exit;

  Options := TStringList.Create;
  msError := TMemoryStream.Create;
  msOutput := TMemoryStream.Create;
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
      msOutput, msError, outError) then
      raise EFileNotFoundException.CreateFmt(rsFileNotFound,
        [P2CConfig.PDFImagesExe]);

    case outError of
      0: ; // OK
      1: ShowMessage('PDFImages: Error opening a PDF file.');
      2: ShowMessage('PDFImages: Error opening an output file.');
      3: ShowMessage('PDFImages: Error related to PDF permissions.');
      99: ShowMessage('PDFImages: Unknown error.');
      else
        ShowMessage('PDFImages: No handled error.');
    end;

    if outError = 0 then
      PDFExtracted := True;

    PrintExtractLog(Options, msOutput, msError);

  finally
    FreeAndNil(Options);
    FreeAndNil(msError);
    FreeAndNil(msOutput);
  end;
end;

procedure TfmP2CMain.PDFExtractFiles(aPDFFile: string);
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

procedure TfmP2CMain.Convert2JPG;

  procedure DoConvert(aFile: string);
  var
    InImage: TPicture;
    OutImage: TJPEGImage;
    oFile: string;
    FileExt: string;
  begin

    if not FileExistsUTF8(aFile) then
      Exit;

    FileExt := ExtractFileExt(aFile);

    // '.jpg' -> Do nothing
    if CompareText(FileExt, '.jpg') = 0 then
      Exit;

    oFile := ChangeFileExt(aFile, '.jpg');

    // '.jpeg' -> Change to '.jpg'
    if CompareText(FileExt, '.jpeg') = 0 then
    begin
      RenameFileUTF8(aFile, oFile);
      mConsoleLog.Lines.Add('Renaming image: ' + aFile);
      Exit;
    end;

    mConsoleLog.Lines.Add('Converting image: ' + aFile);

    InImage := TPicture.Create;
    OutImage := TJPEGImage.Create;
    try
      InImage.LoadFromFile(aFile);

      OutImage.CompressionQuality := 90;
      OutImage.Performance := jpBestQuality;
      OutImage.Assign(InImage.Graphic);
      OutImage.SaveToFile(oFile);

      DeleteFileUTF8(aFile);

    finally
      FreeAndNil(InImage);
      FreeAndNil(OutImage);
    end;
  end;

var
  aFileList: TStringList;
  i: integer;
begin
  mConsoleLog.Lines.Add('');
  mConsoleLog.Lines.Add('CONVERTING TO JPG');
  mConsoleLog.Lines.Add('-----------------');

  aFileList := FindAllFiles(TempFolder, ImageMask);

  i := 0;
  while i < aFileList.Count do
  begin
    DoConvert(aFileList[i]);
    Inc(i);
  end;
  aFileList.Free;
end;

procedure TfmP2CMain.PrintExtractLog(CMDLine: TStringList;
  msOutput, msError: TMemoryStream);
var
  aStrList: TStringList;
begin
  mConsoleLog.Clear;
  mConsoleLog.Lines.Add('COMMAND LINE');
  mConsoleLog.Lines.Add('------------');
  mConsoleLog.Lines.AddStrings(CMDLine, False);

  aStrList := TStringList.Create;

  aStrList.LoadFromStream(msOutput);
  mConsoleLog.Lines.Add('');
  mConsoleLog.Lines.Add('OUTPUT');
  mConsoleLog.Lines.Add('------');
  mConsoleLog.Lines.AddStrings(aStrList, False);

  aStrList.LoadFromStream(msError);
  mConsoleLog.Lines.Add('');
  mConsoleLog.Lines.Add('ERROR');
  mConsoleLog.Lines.Add('-----');
  mConsoleLog.Lines.AddStrings(aStrList, False);
  mConsoleLog.Lines.Add('');

  aStrList.Free;
end;

procedure TfmP2CMain.bExtractPagesClick(Sender: TObject);
var
  aPDFFile: string;
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

procedure TfmP2CMain.bMakeCBXClick(Sender: TObject);
var
  PDFFile: string;
  ZipFile: string;
  ZipType: string;
  i: integer;
begin
  i := 0;
  while i < ePDFFile.DialogFiles.Count do
  begin

  PDFFile := ePDFFile.DialogFiles[i];

  StatusBar.SimpleText := 'Please Wait. Creating CBX: ' + PDFFile;
  Self.Repaint;

  case rgbOutputFormat.ItemIndex of
      1: begin
        ZipFile := ExtractFileNameWithoutExt(ePDFFile.DialogFiles[i]) + '.cb7';
        ZipType := '7z';
      end;
      else
      begin
        ZipFile := ExtractFileNameWithoutExt(ePDFFile.DialogFiles[i]) + '.cbz';
        ZipType := 'zip';
      end;
    end;
    Inc(i);

    // If there is only 1 file its already extracted
    if not PDFExtracted then
      PDFExtractFiles(PDFFile);

    // TODO: Resize to 2048px max

    if chkConvert2JPG.Checked then
    begin
      StatusBar.SimpleText := 'Please Wait. Converting to JPEG';
      Self.Repaint;
      Convert2JPG;
    end;

    w7zCompressFolder(ZipFile, TempFolder, False, True, ZipType);

    ImageList.Clear;
    ClearTempFolder;
    PDFExtracted := False;

    if chkDeletePDF.Checked then
      DeleteFileUTF8(PDFFile);

    StatusBar.SimpleText := 'Finished. Creating CBX: ' + PDFFile;
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
  ImageList.Clear;
end;

procedure TfmP2CMain.bOpenOutputFolderClick(Sender: TObject);
begin
  if ePDFFile.DialogFiles.Count > 0 then
    OpenDocument(ExtractFileDir(ePDFFile.DialogFiles[0]))
  else
  if ePDFFile.FileName <> '' then
    OpenDocument(ExtractFileDir(ePDFFile.FileName));
end;

procedure TfmP2CMain.bReloadImagesClick(Sender: TObject);
begin
  if PDFExtracted then
    LoadImages;
end;

procedure TfmP2CMain.LoadImages;
begin
  ImageList.Clear;
  fmImgListPreview.FileList := nil;
  StatusBar.SimpleText := '';

  // HACK: Warning the user that are some PNM files
  FindAllFiles(ImageList, TempFolder, '*.pnm;*.pbm;*.pgm;*.ppm');
  if ImageList.Count > 0 then
  begin
    if ImageList.Count > 1 then
      StatusBar.SimpleText :=
        'WARNING: There are ' + IntToStr(ImageList.Count) +
        ' PNM files that you may want to convert and then reload the images.'
    else
      StatusBar.SimpleText :=
        'WARNING: There is a PNM file that you may want to convert and then reload the images.';
  end;

  ImageList.Clear;
  FindAllFiles(ImageList, TempFolder, ImageMask);
  fmImgListPreview.FileList := ImageList;
end;

procedure TfmP2CMain.SetP2CConfig(AValue: cP2CConfig);
begin
  if FP2CConfig = AValue then Exit;
  FP2CConfig := AValue;

  LoadFrameData;
end;

procedure TfmP2CMain.SetImageMask(AValue: string);
begin
  if FImageMask = AValue then Exit;
  FImageMask := AValue;
end;

procedure TfmP2CMain.SetPDFExtracted(AValue: boolean);
begin
  FPDFExtracted := AValue;

  bReloadImages.Enabled := PDFExtracted;
  bMakeCBX.Enabled := PDFExtracted;
end;

procedure TfmP2CMain.SetTempFolder(AValue: string);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure TfmP2CMain.Set7zWrapper;
var
  aPath: string;
begin
  w7zSetPathTo7zexe(e7zExecutable.Text);

  // Well try 7zG.exe too
  aPath := SetAsFolder(ExtractFileDir(e7zExecutable.Text)) + '7zG.exe';
  if FileExistsUTF8(aPath) then
    w7zSetPathTo7zGexe(aPath)
  else
    w7zSetPathTo7zGexe('');
end;

function TfmP2CMain.ClearTempFolder: boolean;
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

procedure TfmP2CMain.DoClearFrameData;
begin
  ePDFtoPNGExecutable.Text := '';
  ePDFImagesExecutable.Text := '';
  e7zExecutable.Text := '';
  Set7zWrapper;
  eImgEditorExecutable.Text := '';
  eImgEditorParams.Text := '';
end;

procedure TfmP2CMain.DoLoadFrameData;
begin
  Enabled := assigned(P2CConfig);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  LoadConfig;
end;

constructor TfmP2CMain.Create(TheOwner: TComponent);
var
  i: integer;
begin
  inherited Create(TheOwner);

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;

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
