unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, ExtCtrls, IniFiles, strutils, LCLIntf;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bConvert: TButton;
    bNext: TButton;
    bOpenImage: TButton;
    bOpenTempFolder: TButton;
    bPreview: TButton;
    bPrevious: TButton;
    bSaveConfig: TButton;
    chkDeletePDF: TCheckBox;
    chkPNG: TCheckBox;
    eImageEditorParams: TEdit;
    eOwnerPassword: TEdit;
    ePDFFile: TFileNameEdit;
    ePdfimagesExecutable: TFileNameEdit;
    e7zaExecutable: TFileNameEdit;
    eRarExecutable: TFileNameEdit;
    eImageEditorExecutable: TFileNameEdit;
    eUserPassword: TEdit;
    gbPasswords: TGroupBox;
    iImage: TImage;
    lImageEditorExecutable: TLabel;
    lImageEditorParams: TLabel;
    lIndex: TLabel;
    lNameImage: TLabel;
    lRarExecutable: TLabel;
    lOwnerPassword: TLabel;
    lPDFFile: TLabel;
    lPdfImagesExecutable: TLabel;
    l7zaExecutable: TLabel;
    lUserPassword: TLabel;
    pGuardar: TPanel;
    pImage: TPanel;
    pRarExecutable: TPanel;
    pcMain: TPageControl;
    pLeft: TPanel;
    pOwnerPassword: TPanel;
    pPDFFile: TPanel;
    pOutputFolder: TPanel;
    pPdfimagesExecutable: TPanel;
    p7zaExecutable: TPanel;
    pImageEditorExecutable: TPanel;
    pRight: TPanel;
    pUserPassword: TPanel;
    rgbOutputType: TRadioGroup;
    StatusBar: TStatusBar;
    pagConvert: TTabSheet;
    pagOptions: TTabSheet;
    procedure bConvertClick(Sender: TObject);
    procedure bNextClick(Sender: TObject);
    procedure bOpenImageClick(Sender: TObject);
    procedure bOpenTempFolderClick(Sender: TObject);
    procedure bPreviewClick(Sender: TObject);
    procedure bPreviousClick(Sender: TObject);
    procedure bSaveConfigClick(Sender: TObject);
    procedure ePDFFileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FExtracted: Boolean;
    FImageList: TStringList;
    FIndexImage: Integer;
    FoFileName: String;
    FTmpFolder: String;
    procedure SetExtracted(const AValue: Boolean);
    procedure SetImageList(const AValue: TStringList);
    procedure SetIndexImage(const AValue: Integer);
    procedure SetoFileName(const AValue: String);
    procedure SetTmpFolder(const AValue: String);
  private
    { private declarations }
    property TmpFolder: String read FTmpFolder write SetTmpFolder;
    property Extracted: Boolean read FExtracted write SetExtracted;
    property oFileName: String read FoFileName write SetoFileName;

    property ImageList: TStringList read FImageList write SetImageList;
    property IndexImage: Integer read FIndexImage write SetIndexImage;

    procedure ConvertToPNG;

    procedure ClearImages;
    procedure LoadImages;
    procedure PreviewImages;
    procedure PreviousImage;
    procedure NextImage;

    procedure OpenImageEditor;

    procedure ConvertPDF;
    procedure ExtractFiles(aPDFFile: String);

    procedure LoadConfig;
    procedure SaveConfig;

    function MakeRAR(aTmpFolder: String): String;
    function Make7Z(aTmpFolder: String): String;
    function MakeZIP(aTmpFolder: String): String;
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.ePDFFileChange(Sender: TObject);
begin
  bPreview.Enabled := False;
  bConvert.Enabled := False;
  Extracted := False;
  ClearImages;

  if DirectoryExistsUTF8(TmpFolder) then
    DeleteDirectory(UTF8ToSys(TmpFolder), False);
  ForceDirectory(UTF8ToSys(TmpFolder));

  case ePDFFile.DialogFiles.Count of
    0: ePDFFile.Text := '';
    1:
    begin
      bPreview.Enabled := True;
      bConvert.Enabled := True;
    end;
    else
    begin
      ePDFFile.Text := 'Various files selected';
      bConvert.Enabled := True;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ChDir(ProgramDirectory);
  TmpFolder := IncludeTrailingPathDelimiter(GetTempDir) + 'PDF2CBX';
  ImageList := TStringList.Create;
  pcMain.ActivePageIndex := 0;
  LoadConfig;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DeleteDirectory(UTF8ToSys(TmpFolder), False);
  FreeAndNil(FImageList);
end;

procedure TfrmMain.SetExtracted(const AValue: Boolean);
begin
  if FExtracted = AValue then
    exit;
  FExtracted := AValue;
end;

procedure TfrmMain.SetImageList(const AValue: TStringList);
begin
  if FImageList = AValue then
    exit;
  FImageList := AValue;
end;

procedure TfrmMain.SetIndexImage(const AValue: Integer);
begin
  if FIndexImage = AValue then
    exit;
  FIndexImage := AValue;
end;

procedure TfrmMain.SetoFileName(const AValue: String);
begin
  if FoFileName = AValue then
    exit;
  FoFileName := AValue;
end;

procedure TfrmMain.SetTmpFolder(const AValue: String);
begin
  FTmpFolder := IncludeTrailingPathDelimiter(AValue);
end;

procedure TfrmMain.ConvertToPNG;

  procedure C2P(aFile: String);
  var
    PAMG: TPortableAnyMapGraphic;
    PNG: TPortableNetworkGraphic;
    oFile: String;
  begin
    if not FileExistsUTF8(aFile) then
      Exit;

    StatusBar.SimpleText := 'Converting image: ' + aFile;
    Self.Invalidate;

    oFile := ChangeFileExt(aFile, '.png');

    PAMG := TPortableAnyMapGraphic.Create;
    PNG := TPortableNetworkGraphic.Create;
    try
      PAMG.LoadFromFile(aFile);
      PNG.Assign(PAMG);
      PNG.SaveToFile(oFile);
      DeleteFileUTF8(aFile);
    finally
      FreeAndNil(PAMG);
      FreeAndNil(PNG);
    end;
  end;

  //  procedure TfrmMain.ConvertToPNG;
var
  Info: TSearchRec;
  sExt: String;
  i: Integer;
begin
  for i := 1 to 3 do
  begin
    case i of
      1: sExt := '.pgm';
      2: sExt := '.pbm';
      3: sExt := '.ppm';
    end;

    if FindFirstUTF8(TmpFolder + '*' + sExt, 0, Info) = 0 then
      try
        repeat
          if ((Info.Attr and faDirectory) = 0) then
            C2P(TmpFolder + Info.Name);
        until (FindNext(Info) <> 0);
      finally
        SysUtils.FindClose(Info);
      end;
  end;
end;

procedure TfrmMain.ClearImages;
begin
  ImageList.Clear;
  IndexImage := -1;
  iImage.Picture.Clear;
  lIndex.Caption := '0 / 0';
  lNameImage.Caption := '';
end;

procedure TfrmMain.LoadImages;
var
  Info: TSearchRec;
begin
  ClearImages;

  if FindFirstUTF8(TmpFolder + AllFilesMask, 0, Info) = 0 then
    try
      repeat
        ImageList.Add(TmpFolder + Info.Name);
      until (FindNext(Info) <> 0);
    finally
      SysUtils.FindClose(Info);
    end;

  IndexImage := -1;
  NextImage;
end;

procedure TfrmMain.PreviewImages;
begin
  if ePDFFile.DialogFiles.Count <> 1 then
    Exit;

  if not Extracted then
    ExtractFiles(ePDFFile.DialogFiles[0]);

  LoadImages;
end;

procedure TfrmMain.PreviousImage;
begin
  if not Extracted then
    Exit;

  Dec(FIndexImage);

  if IndexImage < 0 then
    IndexImage := ImageList.Count - 1;

  iImage.Picture.LoadFromFile(ImageList[IndexImage]);
  lNameImage.Caption := ExtractFileName(ImageList[IndexImage]);
  lIndex.Caption := IntToStr(IndexImage + 1) + ' / ' +
    IntToStr(ImageList.Count);
end;

procedure TfrmMain.NextImage;
begin
  if not Extracted then
    Exit;

  Inc(FIndexImage);

  if IndexImage >= ImageList.Count then
    IndexImage := 0;

  iImage.Picture.LoadFromFile(ImageList[IndexImage]);
  lNameImage.Caption := ExtractFileName(ImageList[IndexImage]);
  lIndex.Caption := IntToStr(IndexImage + 1) + ' / ' +
    IntToStr(ImageList.Count);
end;

procedure TfrmMain.OpenImageEditor;
var
  Params: String;
begin
  if not FileExistsUTF8(eImageEditorExecutable.FileName) then
    Exit;

  Params := AnsiReplaceStr(eImageEditorParams.Text, '%FILE%',
    ImageList[IndexImage]);
  Params := AnsiReplaceStr(Params, '%FILENAME%',
    ExtractFileName(ImageList[IndexImage]));
  Params := AnsiReplaceStr(Params, '%FOLDER%',
    ExtractFileDir(ImageList[IndexImage]));
  ExecuteProcess(UTF8ToSys(eImageEditorExecutable.FileName), Params);

  LoadImages;
end;

procedure TfrmMain.ConvertPDF;
var
  PDFFile: String;
  ZipFile: String;
  i: Integer;
begin
  i := 0;
  while i < ePDFFile.DialogFiles.Count do
  begin
    PDFFile := ePDFFile.DialogFiles[i];
    Inc(i);

    if not Extracted then
      ExtractFiles(PDFFile);

    case rgbOutputType.ItemIndex of
      0: ZipFile := MakeRAR(TmpFolder); // RAR
      1: ZipFile := Make7Z(TmpFolder);  // 7Z
      2: ZipFile := MakeZIP(TmpFolder); // ZIP
    end;

    RenameFileUTF8(ZipFile, ExtractFilePath(PDFFile) +
      ExtractFileName(ZipFile));

    ClearImages;
    DeleteDirectory(UTF8ToSys(TmpFolder), False);
    ForceDirectory(UTF8ToSys(TmpFolder));
    Extracted := False;

    if chkDeletePDF.Checked then
      DeleteFileUTF8(PDFFile);
  end;

  // Si lo hemos borrado lo quitamos del editbox
  if chkDeletePDF.Checked then
    ePDFFile.FileName := '';
end;

procedure TfrmMain.ExtractFiles(aPDFFile: String);
var
  Options: String;
begin
  if not FileExistsUTF8(aPDFFile) then
    Exit;

  oFileName := ExtractFileNameOnly(aPDFFile);

  StatusBar.SimpleText := 'Extracting images from: ' + aPDFFile;

  Options := '';
  if eOwnerPassword.Text <> '' then;
  Options := Options + '-opw ' + eOwnerPassword.Text + ' ';

  if eOwnerPassword.Text <> '' then;
  Options := Options + '-upw ' + eUserPassword.Text + ' ';

  ExecuteProcess(UTF8ToSys('pdfimages'), '-j ' + Options +
    '"' + aPDFFile + '" "' + TmpFolder + oFileName + '"');

  Self.Invalidate;

  if chkPNG.Checked then
    ConvertToPNG;

  Extracted := True;
end;

procedure TfrmMain.LoadConfig;
var
  ConfigFile: String;
  IniFile: TMemIniFile;
begin
  ConfigFile := ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), '.ini');

  if not FileExistsUTF8(ConfigFile) then
    Exit;
  IniFile :=
    TMemIniFile.Create(UTF8ToSys(ConfigFile));
  try
    ePdfimagesExecutable.FileName :=
      IniFile.ReadString('Executables', 'pdfimages',
      ePdfimagesExecutable.FileName);
    e7zaExecutable.FileName :=
      IniFile.ReadString('Executables', '7za', e7zaExecutable.FileName);
    eRarExecutable.FileName :=
      IniFile.ReadString('Executables', 'rar', eRarExecutable.FileName);
    eImageEditorExecutable.FileName :=
      IniFile.ReadString('Executables', 'ImageEditor',
      eImageEditorExecutable.FileName);
    eImageEditorParams.Text :=
      IniFile.ReadString('Executables', 'ImageEditorParams',
      eImageEditorParams.Text);
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TfrmMain.SaveConfig;
var
  IniFile: TMemIniFile;
begin
  IniFile := TMemIniFile.Create(
    UTF8ToSys(ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), '.ini')));
  try
    IniFile.WriteString('Executables', 'pdfimages',
      ePdfimagesExecutable.FileName);
    IniFile.WriteString('Executables', '7za', e7zaExecutable.FileName);
    IniFile.WriteString('Executables', 'rar', eRarExecutable.FileName);
    IniFile.WriteString('Executables', 'ImageEditor',
      eImageEditorExecutable.FileName);
    IniFile.WriteString('Executables', 'ImageEditorParams',
      eImageEditorParams.Text);

    IniFile.UpdateFile;
  finally
    FreeAndNil(IniFile);
  end;
end;

function TfrmMain.MakeRAR(aTmpFolder: String): String;
begin
  Result := aTmpFolder + oFileName + '.cbr';
  ExecuteProcess(UTF8ToSys(eRarExecutable.FileName), 'a -ep ' +
    '"' + Result + '" "' + aTmpFolder + '*.*"');
end;

function TfrmMain.Make7Z(aTmpFolder: String): String;
begin
  Result := aTmpFolder + oFileName + '.cb7';
  ExecuteProcess(UTF8ToSys(e7zaExecutable.FileName), 'a -mx9 -t7z ' +
    '"' + Result + '" "' + aTmpFolder + '*.*"');
end;

function TfrmMain.MakeZIP(aTmpFolder: String): String;
begin
  Result := aTmpFolder + oFileName + '.cbz';
  ExecuteProcess(UTF8ToSys(e7zaExecutable.FileName), 'a -mx9 -tzip ' +
    '"' + Result + '" "' + aTmpFolder + '*.*"');
end;

procedure TfrmMain.bConvertClick(Sender: TObject);
begin
  ConvertPDF;
end;

procedure TfrmMain.bNextClick(Sender: TObject);
begin
  NextImage;
end;

procedure TfrmMain.bOpenImageClick(Sender: TObject);
begin
  OpenImageEditor;
end;

procedure TfrmMain.bOpenTempFolderClick(Sender: TObject);
begin
  OpenDocument(TmpFolder);
end;

procedure TfrmMain.bPreviewClick(Sender: TObject);
begin
  PreviewImages;
end;

procedure TfrmMain.bPreviousClick(Sender: TObject);
begin
  PreviousImage;
end;

procedure TfrmMain.bSaveConfigClick(Sender: TObject);
begin
  SaveConfig;
end;

end.

