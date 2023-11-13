unit ufrIconBorderMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, FileUtil, BGRABitmap, BGRABitmapTypes, Math, Types, lclintf,
  EditBtn, Spin, ColorBox, LazFileUtils, LCLTranslator, ActnList,
  // Misc units
  uVersionSupport,
  // CHX units
  uCHXConst, uCHXRscStr, uCHXDlgUtils, uCHXFileUtils, uCHXStrUtils,
  // CHX classes
  ucEIBConfig,
  // CHX forms
  ufrCHXForm;

{ TODO:
    - Move logic to CHXFrames.
    - Make ResourceStrings or constant strings.
    - Make a shared Color Button and Transparency SpinEdit for Fill and Paint
      (although sometimes is usefull to have separated ones).
    - Better logic with pixel color picking (Maybe don't wait to mouse button
      is released).
}

const
  krsEIBName = 'ETKIconBorder';

resourcestring
  rsFillColor = 'L-Click: Fill with color. | R-Click: Fill while dragging. | '
    + 'M-Click: Pick color from pixel.';
  rsHintPaintingPixel = 'Drag to paint with desired color and transparency.';
  rsHintPaintPixel =
    'L-Click: Paint a pixel. | R-Click: Pick color from pixel.';
  rsHintPickingPaintColor = 'Color is selected when click release.';
  rsHintReplaceColor = 'L-Click: Replace clicked color with desired color and '
    + 'transparency.';
  rsHintReplacingColor = 'Color is replaced when click is finished.';
  rsHintSelect = 'Drag to select a rectangle. | R-Click: Make transparent. | '
    + 'M-Click: Cut.';
  rsHintSelecting = 'Drag until desired size.';

type

  TMouseActionInput = (
    maiSelectRect,         // Selecting a rectangle
    maiSelectingRect,
    maiPaintPixel,         // Removing pixels by hand
    maiPaintingPixel,
    maiPickingPaintColor,
    maiFillColor,          // Fill near pixels by color
    maiFillingColor,
    maiReplaceColor,
    maiReplacingColor      //Replacing color in full image
    );

  TProcessOutputFilter = (
    pofEmutecaIconBorder,         // Emuteca border style icons
    pofRemoveIconBorder           // Remove semitransparent pixels
    );


  { TfrmIconBorder }

  TfrmIconBorder = class(TfrmCHXForm)
    actAddFile : TAction;
    actAddFolder : TAction;
    actClearList : TAction;
    actDeleteFile : TAction;
    actSaveOutput : TAction;
    actOpenOutDir : TAction;
    actReplaceInpFile : TAction;
    actReplaceFile : TAction;
    actOpenInpDir : TAction;
    actRemoveItem : TAction;
    alMain : TActionList;
    bAddFile : TButton;
    bAddFolder : TButton;
    bAutoZoomInput : TButton;
    bAutoZoomOutput : TButton;
    bClearList : TButton;
    bColorFillInput : TColorButton;
    bColorPaintInput : TColorButton;
    bColorBorderEmutecaIcon : TColorButton;
    bColorReplaceInput : TColorButton;
    bCutSelectionInput : TButton;
    bDefaultIconBorder : TButton;
    bBlackIconBorder : TButton;
    bDeleteInputFile : TButton;
    bFlipHInput : TButton;
    bFlipVInput : TButton;
    bOpenInputDir : TButton;
    bOpenOutputDir : TButton;
    bProcessOutput : TButton;
    bRemoveItem : TButton;
    bReplaceInputFile : TButton;
    bRotateCCWInput : TButton;
    bRotateCWInput : TButton;
    bSaveInput : TButton;
    bSaveOutput : TButton;
    bSelectionTransparentInput : TButton;
    bTransparentPaint : TButton;
    bTransparentFill : TButton;
    bTransparentReplace : TButton;
    bZoom1xOutput : TButton;
    bZoomInInput : TButton;
    bZoom1xInput : TButton;
    bZoomInOutput : TButton;
    bZoomOutInput : TButton;
    bZoomOutOutput : TButton;
    cbxColorBackground : TColorBox;
    chkAutoCropTransparency : TCheckBox;
    chkCopyReplaceToFill : TCheckBox;
    chkDiagonalNeightbours : TCheckBox;
    chkOverwriteOutput : TCheckBox;
    chkRemoveTransEmutecaIcon : TCheckBox;
    eOpacityReplaceInput : TSpinEdit;
    eToleranceFillInput : TSpinEdit;
    eOpacityPaintInput : TSpinEdit;
    eOpacityBorderEmutecaIcon : TSpinEdit;
    eOpacityFillInput : TSpinEdit;
    eOutputFolder : TDirectoryEdit;
    FileList : TListBox;
    gbxFileInput : TGroupBox;
    gbxFileOutput : TGroupBox;
    gbxOutputImage : TGroupBox;
    gbxInputImage : TGroupBox;
    gbxTransformInput : TGroupBox;
    gbxZoomInput : TGroupBox;
    gbxZoomOuput : TGroupBox;
    lColorPaintInput : TLabel;
    lColorFillInput : TLabel;
    lColorReplaceInput : TLabel;
    lOpacityPaintInput : TLabel;
    lOpacityFillInput : TLabel;
    lOpacityReplaceInput : TLabel;
    lOutputFolder : TLabel;
    lToleranceFillInput : TLabel;
    lZoomInput : TLabel;
    lZoomOutput : TLabel;
    OpenFilesDialog : TOpenDialog;
    pButtonsFile : TPanel;
    pbxOutputImage : TPaintBox;
    pgcImageInput : TPageControl;
    pgcImageOutput : TPageControl;
    pOptions : TPanel;
    pToolsInput : TPanel;
    pZoomInput : TPanel;
    gbxLeft : TGroupBox;
    pbxInputImage : TPaintBox;
    pToolsOutput : TPanel;
    pZoomOutput : TPanel;
    rgbBackGround : TRadioGroup;
    rgbRemoveTrans : TRadioGroup;
    sbxInputImage : TScrollBox;
    sbxOutputImage : TScrollBox;
    SelectDirectoryDialog : TSelectDirectoryDialog;
    Splitter1 : TSplitter;
    Splitter2 : TSplitter;
    StatusBar : TStatusBar;
    pagCommonInput : TTabSheet;
    pagSelectInput : TTabSheet;
    pagPaintInput : TTabSheet;
    pagFillInput : TTabSheet;
    pagEmutecaIconBorder : TTabSheet;
    pagRemoveEmutecaBorder : TTabSheet;
    pagReplaceInput : TTabSheet;

    procedure actAddFileExecute(Sender : TObject);
    procedure actAddFolderExecute(Sender : TObject);
    procedure actClearListExecute(Sender : TObject);
    procedure actDeleteFileExecute(Sender : TObject);
    procedure actOpenInpDirExecute(Sender : TObject);
    procedure actOpenOutDirExecute(Sender : TObject);
    procedure actRemoveItemExecute(Sender : TObject);
    procedure actReplaceFileExecute(Sender : TObject);
    procedure actReplaceInpFileExecute(Sender : TObject);
    procedure actSaveOutputExecute(Sender : TObject);
    procedure bAutoZoomInputClick(Sender : TObject);
    procedure bAutoZoomOutputClick(Sender : TObject);
    procedure bBlackIconBorderClick(Sender : TObject);
    procedure bCutSelectionInputClick(Sender : TObject);
    procedure bDefaultIconBorderClick(Sender : TObject);
    procedure bFlipHInputClick(Sender : TObject);
    procedure bFlipVInputClick(Sender : TObject);
    procedure bProcessOutputClick(Sender : TObject);
    procedure bRotateCCWInputClick(Sender : TObject);
    procedure bRotateCWInputClick(Sender : TObject);
    procedure bSelectionTransparentInputClick(Sender : TObject);
    procedure bTransparentFillClick(Sender : TObject);
    procedure bTransparentReplaceClick(Sender : TObject);
    procedure bTransparentPaintClick(Sender : TObject);
    procedure bZoom1xInputClick(Sender : TObject);
    procedure bZoom1xOutputClick(Sender : TObject);
    procedure bZoomInInputClick(Sender : TObject);
    procedure bZoomInOutputClick(Sender : TObject);
    procedure bZoomOutInputClick(Sender : TObject);
    procedure bZoomOutOutputClick(Sender : TObject);
    procedure cbxColorBackgroundChange(Sender : TObject);
    procedure eOpacityFillInputChange(Sender : TObject);
    procedure eOpacityPaintInputChange(Sender : TObject);
    procedure eOpacityReplaceInputChange(Sender : TObject);
    procedure eOutputFolderChange(Sender : TObject);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FileListClick(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure pbxInputImageMouseDown(Sender : TObject;
      Button : TMouseButton; Shift : TShiftState; X, Y : integer);
    procedure pbxInputImageMouseLeave(Sender : TObject);
    procedure pbxInputImageMouseMove(Sender : TObject;
      Shift : TShiftState; X, Y : integer);
    procedure pbxInputImageMouseUp(Sender : TObject;
      Button : TMouseButton; Shift : TShiftState; X, Y : integer);
    procedure pbxInputImagePaint(Sender : TObject);
    procedure pbxOutputImageMouseDown(Sender : TObject;
      Button : TMouseButton; Shift : TShiftState; X, Y : integer);
    procedure pbxOutputImageMouseMove(Sender : TObject;
      Shift : TShiftState; X, Y : integer);
    procedure pbxOutputImageMouseUp(Sender : TObject;
      Button : TMouseButton; Shift : TShiftState; X, Y : integer);
    procedure pbxOutputImagePaint(Sender : TObject);
    procedure pgcImageInputChange(Sender : TObject);
    procedure pgcImageOutputChange(Sender : TObject);
    procedure rgbBackGroundSelectionChanged(Sender : TObject);

  private
    FActualInputImage : TBGRABitmap;
    FActualOutputImage : TBGRABitmap;
    FEIBConfig : cEIBConfig;
    FFocusRectInput : TRect;
    FMouseActionInput : TMouseActionInput;
    FProcessOutputFilter : TProcessOutputFilter;
    FSelectionInput : TRect;
    FVisibleInputImage : TBGRABitmap;
    FVisibleOutputImage : TBGRABitmap;
    FXOffset : LongInt;
    FYOffset : LongInt;
    FZoomInput : integer;
    FZoomOutput : integer;

    procedure SetEIBConfig(AValue : cEIBConfig);
    procedure SetFocusRectInput(AValue : TRect);
    procedure SetMouseActionInput(AValue : TMouseActionInput);
    procedure SetProcessOutputFilter(AValue : TProcessOutputFilter);
    procedure SetSelectionInput(AValue : TRect);
    procedure SetXOffset(AValue : LongInt);
    procedure SetYOffset(AValue : LongInt);
    procedure SetZoomInput(AValue : integer);
    procedure SetZoomOutput(AValue : integer);

  protected
    property EIBConfig : cEIBConfig read FEIBConfig write SetEIBConfig;

    property XOffset : LongInt read FXOffset write SetXOffset;
    property YOffset : LongInt read FYOffset write SetYOffset;
    { Offset diference between Input image and Output image.

      if we click on output image we can know what pixels is in the input image.
    }

    procedure DrawImageInput;
    procedure DrawImageOutput;

    procedure AutoZoomInput;
    procedure AutoZoomOutput;

    procedure RemovePixelInput(const X, Y : integer);
    procedure RemoveSameColorNeighboursInput(X, Y : integer;
      ToColor : TBGRAPixel; Tolerance : byte);

    procedure DoImageMouseMove(Shift : TShiftState; ImgX, ImgY : LongInt);
    procedure DoImageMouseUp(Button : TMouseButton; Shift : TShiftState;
      ImgX, ImgY : LongInt);
    procedure DoImageMouseDown(Button : TMouseButton;
      Shift : TShiftState; ImgX, ImgY : LongInt);

    function SelectionZoomInput : TRect;

  public
    property ActualInputImage : TBGRABitmap read FActualInputImage;
    property ActualOutputImage : TBGRABitmap read FActualOutputImage;
    {< Actual images. }

    property VisibleInputImage : TBGRABitmap read FVisibleInputImage;
    property VisibleOutputImage : TBGRABitmap read FVisibleOutputImage;
    {< Visible images with zoom, selection, effects... }

    property ZoomInput : integer read FZoomInput write SetZoomInput;
    property ZoomOutput : integer read FZoomOutput write SetZoomOutput;

    property SelectionInput : TRect read FSelectionInput
      write SetSelectionInput;

    property MouseActionInput : TMouseActionInput
      read FMouseActionInput write SetMouseActionInput;

    property ProcessOutputFilter : TProcessOutputFilter
      read FProcessOutputFilter write SetProcessOutputFilter;

  end;

function GCD(a, b : integer) : integer;

var
  frmIconBorder : TfrmIconBorder;

implementation

function GCD(a, b : integer) : integer;
var
  temp : integer;
begin
  while b <> 0 do
  begin
    temp := b;
    b := a mod b;
    a := temp;
  end;
  Result := a;
end;

{$R *.lfm}

{ TfrmIconBorder }
procedure TfrmIconBorder.SetEIBConfig(AValue : cEIBConfig);
begin
  if FEIBConfig = AValue then Exit;
  FEIBConfig := AValue;
end;

procedure TfrmIconBorder.SetFocusRectInput(AValue : TRect);
begin
  if FFocusRectInput = AValue then
    Exit;
  FFocusRectInput := AValue;
end;

procedure TfrmIconBorder.SetMouseActionInput(AValue : TMouseActionInput);
var
  aHint : string;
begin
  if FMouseActionInput = AValue then
    Exit;
  FMouseActionInput := AValue;

  case MouseActionInput of
    maiSelectRect: aHint := rsHintSelect;
    maiSelectingRect: aHint := rsHintSelecting;
    maiPaintPixel: aHint := rsHintPaintPixel;
    maiPaintingPixel: aHint := rsHintPaintingPixel;
    maiPickingPaintColor: aHint := rsHintPickingPaintColor;
    maiFillColor, maiFillingColor: aHint := rsFillColor;
    maiReplaceColor: aHint := rsHintReplaceColor;
    maiReplacingColor: aHint := rsHintReplacingColor;
    else
      aHint := '';
  end;

  StatusBar.Panels[3].Text := aHint;
end;

procedure TfrmIconBorder.SetProcessOutputFilter(AValue : TProcessOutputFilter);
begin
  if FProcessOutputFilter = AValue then
    Exit;
  FProcessOutputFilter := AValue;
end;

procedure TfrmIconBorder.SetZoomInput(AValue : integer);
begin
  if not Assigned(ActualInputImage) then
  begin
    FZoomInput := 1;
    lZoomInput.Caption := Format('%dx', [ZoomInput]);
    Exit;
  end;

  // Checking very high zoom
  while (max(ActualInputImage.Width, ActualInputImage.Height) * AVAlue) >
    (2 ** 14) do
    Dec(AValue);

  if AValue <= 0 then
    AValue := 1;

  FZoomInput := AValue;

  lZoomInput.Caption := Format('%dx', [ZoomInput]);

  DrawImageInput;
end;

procedure TfrmIconBorder.SetZoomOutput(AValue : integer);
begin
  if not Assigned(ActualOutputImage) then
  begin
    FZoomOutput := 1;
    Exit;
  end;

  // Checking very high zoom
  while (max(ActualOutputImage.Width, ActualOutputImage.Height) * AVAlue) >
    (2 ** 14) do
    Dec(AValue);

  if AValue <= 0 then
    AValue := 1;

  FZoomOutput := AValue;

  lZoomOutput.Caption := Format('%dx', [ZoomOutput]);

  DrawImageOutput;
end;

procedure TfrmIconBorder.SetSelectionInput(AValue : TRect);
begin
  if FSelectionInput = AValue then
    Exit;
  FSelectionInput := AValue;
end;

procedure TfrmIconBorder.SetXOffset(AValue : LongInt);
begin
  if FXOffset = AValue then Exit;
  FXOffset := AValue;
end;

procedure TfrmIconBorder.SetYOffset(AValue : LongInt);
begin
  if FYOffset = AValue then Exit;
  FYOffset := AValue;
end;

procedure TfrmIconBorder.DrawImageInput;
var
  Temp : TBGRABitmap;
  ZWidth, ZHeight, ZCheck : LongInt;
begin
  FreeAndNil(FVisibleInputImage);

  if not Assigned(ActualInputImage) then
  begin
    pbxInputImage.ClientWidth := 0;
    pbxInputImage.ClientHeight := 0;
    Exit;
  end;

  ZWidth := ActualInputImage.Width * ZoomInput;
  ZHeight := ActualInputImage.Height * ZoomInput;

  if (pbxInputImage.ClientWidth <> ZWidth) or
    (pbxInputImage.ClientHeight <> ZHeight) then
  begin
    pbxInputImage.ClientWidth := ZWidth;
    pbxInputImage.ClientHeight := ZHeight;
  end;

  FVisibleInputImage := TBGRABitmap.Create(ZWidth, ZHeight);

  case rgbBackGround.ItemIndex of
    1: ; // Transparent
    2: // Color
      VisibleInputImage.Fill(ColorToBGRA(cbxColorBackground.Selected));
    else
    begin
      // Checker background
      ZCheck := max(6, ZoomInput shr 1); // div 2

      VisibleInputImage.DrawCheckers(Rect(0, 0, ZWidth, ZHeight),
        BGRA(192, 192, 192), BGRA(160, 160, 160), ZCheck, ZCheck);
    end;
  end;

  Temp := ActualInputImage.Resample(ActualInputImage.Width *
    ZoomInput, ActualInputImage.Height * ZoomInput, rmSimpleStretch);

  VisibleInputImage.PutImage(0, 0, Temp, dmDrawWithTransparency);

  Temp.Free;

  if not SelectionInput.isEmpty then
    VisibleInputImage.Rectangle(SelectionZoomInput,
      BGRA(255, 0, 255), dmSet);

  pbxInputImage.Invalidate;
end;

procedure TfrmIconBorder.DrawImageOutput;
var
  Temp : TBGRABitmap;
  ZWidth, ZHeight, ZCheck : integer;
begin
  FreeAndNil(FVisibleOutputImage);

  if not Assigned(ActualOutputImage) then
  begin
    pbxOutputImage.ClientWidth := 0;
    pbxOutputImage.ClientHeight := 0;
    Exit;
  end;

  ZWidth := ActualOutputImage.Width * ZoomOutput;
  ZHeight := ActualOutputImage.Height * ZoomOutput;

  if (pbxOutputImage.ClientWidth <> ZWidth) or
    (pbxOutputImage.ClientHeight <> ZHeight) then
  begin
    pbxOutputImage.ClientWidth := ZWidth;
    pbxOutputImage.ClientHeight := ZHeight;
  end;

  FVisibleOutputImage := TBGRABitmap.Create(ZWidth, ZHeight);

  case rgbBackGround.ItemIndex of
    1: ; // Transparent
    2: // Color
      VisibleOutputImage.Fill(ColorToBGRA(cbxColorBackground.Selected));
    else
    begin
      // Checker background
      ZCheck := max(6, ZoomOutput shr 1); // div 2

      VisibleOutputImage.DrawCheckers(Rect(0, 0, ZWidth, ZHeight),
        BGRA(192, 192, 192), BGRA(160, 160, 160), ZCheck, ZCheck);
    end;

  end;

  Temp := ActualOutputImage.Resample(ActualOutputImage.Width *
    ZoomOutput, ActualOutputImage.Height * ZoomOutput, rmSimpleStretch);

  VisibleOutputImage.PutImage(0, 0, Temp, dmDrawWithTransparency);

  Temp.Free;

  //if not SelectionInput.isEmpty then
  //  VisibleInputImage.Rectangle(SelectionZoomInput,
  //    BGRA(255, 0, 255), dmSet);

  pbxOutputImage.Invalidate;
end;

procedure TfrmIconBorder.AutoZoomInput;
var
  i : integer;
begin
  if not assigned(ActualInputImage) then
    Exit;

  i := Min(sbxInputImage.ClientWidth div ActualInputImage.Width,
    sbxInputImage.ClientHeight div ActualInputImage.Height);
  if i < 1 then
    i := 1;

  ZoomInput := i;
end;

procedure TfrmIconBorder.AutoZoomOutput;
var
  i : integer;
begin
  if not assigned(ActualOutputImage) then
    Exit;

  i := Min(sbxOutputImage.ClientWidth div ActualOutputImage.Width,
    sbxOutputImage.ClientHeight div ActualOutputImage.Height);
  if i < 1 then
    i := 1;

  ZoomOutput := i;
end;

procedure TfrmIconBorder.RemovePixelInput(const X, Y : integer);
var
  Pix : PBGRAPixel;
begin
  if (not InRange(X, 0, ActualInputImage.Width - 1)) or
    (not InRange(Y, 0, ActualInputImage.Height - 1)) then
    Exit;

  Pix := ActualInputImage.Scanline[Y] + X;

  // Full transparency -> Black
  if eOpacityPaintInput.Value = 0 then
    Pix^ := BGRAPixelTransparent
  else
    Pix^.FromColor(bColorPaintInput.ButtonColor, eOpacityPaintInput.Value);

  ActualInputImage.InvalidateBitmap;
  DrawImageInput;
end;

procedure TfrmIconBorder.RemoveSameColorNeighboursInput(X, Y : integer;
  ToColor : TBGRAPixel; Tolerance : byte);

{ Recursive can easy create a Stack Overflow error
if (not InRange(X, 0, ActualInputImage.Width - 1)) or
  (not InRange(Y, 0, ActualInputImage.Height - 1)) then
  Exit;

if FromColor = ToColor then
  Exit;

aPixel := ActualInputImage.ScanLine[y] + X;

if not (aPixel^ = FromColor) then
  Exit;

if ToColor.alpha = 0 then
  aPixel^ := BGRAPixelTransparent
else
  aPixel^ := ToColor;

DiagFloodFill(FromColor, ToColor, X - 1, Y);
DiagFloodFill(FromColor, ToColor, X, Y - 1);
DiagFloodFill(FromColor, ToColor, X + 1, Y);
DiagFloodFill(FromColor, ToColor, X, Y + 1);
DiagFloodFill(FromColor, ToColor, X - 1, Y - 1);
DiagFloodFill(FromColor, ToColor, X - 1, Y + 1);
DiagFloodFill(FromColor, ToColor, X + 1, Y - 1);
DiagFloodFill(FromColor, ToColor, X + 1, Y + 1);
}

  procedure DiagFloodFill(X, Y : integer; ToColor : TBGRAPixel;
    Tolerance : byte);
  var
    S : TBGRAPixel;
    SX, EX, I : integer;
    Added : boolean;

    Visited : array of longword;
    VisitedLineSize : integer;

    Stack : array of integer;
    StackCount : integer;
    pScan : PBGRAPixel;

    function CheckPixel(AX, AY : integer) : boolean; inline;
    begin
      if Visited[AX shr 5 + AY * VisitedLineSize] and
        (1 shl (AX and 31)) <> 0 then
        Result := False
      else
      begin
        if (pScan + AX)^ = S then
          Result := True
        else
          Result := FastBGRALinearDiff((pScan + AX)^, S) <= Tolerance;
        // Result := BGRADiff((pScan + AX)^, S) <= Tolerance;
      end;
    end;

    procedure SetVisited(X1, AY, X2 : integer);
    var
      StartMask, EndMask : longword;
      StartPos, EndPos : integer;
    begin
      if X2 < X1 then
        exit;
      StartMask := $FFFFFFFF shl (X1 and 31);
      case X2 and 31 of
        31: EndMask := $FFFFFFFF;
        30: EndMask := $7FFFFFFF;
        else
          EndMask := 1 shl ((X2 and 31) + 1) - 1;
      end;
      StartPos := X1 shr 5 + AY * VisitedLineSize;
      EndPos := X2 shr 5 + AY * VisitedLineSize;
      if StartPos = EndPos then
        Visited[StartPos] := Visited[StartPos] or (StartMask and EndMask)
      else
      begin
        Visited[StartPos] := Visited[StartPos] or StartMask;
        Visited[EndPos] := Visited[EndPos] or EndMask;
        if EndPos - StartPos > 1 then
          FillDWord(Visited[StartPos + 1], EndPos - StartPos - 1, $FFFFFFFF);
      end;
    end;

    procedure Push(AX, AY : integer); inline;
    begin
      if Succ(StackCount) >= High(Stack) then
        SetLength(Stack, Length(Stack) shl 1);

      Stack[StackCount] := AX;
      Inc(StackCount);
      Stack[StackCount] := AY;
      Inc(StackCount);
    end;

    procedure Pop(var AX, AY : integer); inline;
    begin
      Dec(StackCount);
      AY := Stack[StackCount];
      Dec(StackCount);
      AX := Stack[StackCount];
    end;

  begin
    S := (ActualInputImage.ScanLine[Y] + X)^;

    VisitedLineSize := (Width + 31) shr 5;
    SetLength(Visited, VisitedLineSize * Height);
    FillDWord(Visited[0], Length(Visited), 0);

    SetLength(Stack, 2);
    StackCount := 0;

    Push(X, Y);
    repeat
      Pop(X, Y);
      pScan := ActualInputImage.ScanLine[Y];
      if not CheckPixel(X, Y) then
        Continue;

      SX := X;
      while (SX > 0) and CheckPixel(Pred(SX), Y) do
        Dec(SX);
      EX := X;
      while (EX < Pred(ActualInputImage.Width)) and CheckPixel(Succ(EX), Y) do
        Inc(EX);

      SetVisited(SX, Y, EX);

      ActualInputImage.SetHorizLine(SX, Y, EX, ToColor);

      // Diagonals
      if SX > 0 then
        Dec(SX);
      if EX < Pred(ActualInputImage.Width) then
        Inc(EX);

      Added := False;
      if Y > 0 then
      begin
        pScan := ActualInputImage.ScanLine[Pred(Y)];
        for I := SX to EX do
          if CheckPixel(I, Pred(Y)) then
          begin
            if Added then //do not add twice the same segment
              Continue;
            Push(I, Pred(Y));
            Added := True;
          end
          else
            Added := False;
      end;

      Added := False;
      if Y < Pred(ActualInputImage.Height) then
      begin
        pScan := ActualInputImage.ScanLine[Succ(Y)];
        for I := SX to EX do
          if CheckPixel(I, Succ(Y)) then
          begin
            if Added then //do not add twice the same segment
              Continue;
            Push(I, Succ(Y));
            Added := True;
          end
          else
            Added := False;
      end;
    until StackCount <= 0;
  end;

begin
  if not chkDiagonalNeightbours.Checked then
  begin
    // TBGRABitmap.FloodFill don't check Diagonal Neightbours
    ActualInputImage.FloodFill(X, Y, ToColor, fmSet, Tolerance);
  end
  else
  begin
    // A copy of TBGRABitmap.FloodFill that checks Diagonal Neightbours
    DiagFloodFill(X, Y, ToColor, Tolerance);
  end;
end;

procedure TfrmIconBorder.DoImageMouseMove(Shift : TShiftState;
  ImgX, ImgY : LongInt);
var
  CurrSelection : TRect;
  aPixel : PBGRAPixel;
begin
  aPixel := nil;

  StatusBar.Panels[0].Text := Format('%5d, %5d', [ImgX, ImgY]);

  if InRange(ImgX, 0, ActualInputImage.Width - 1) and
    InRange(ImgY, 0, ActualInputImage.Height - 1) then
  begin
    aPixel := ActualInputImage.ScanLine[ImgY] + ImgX;
    StatusBar.Panels[1].Text :=
      Format('%3d, %3d, %3d, %3d', [aPixel^.red, aPixel^.green,
      aPixel^.blue, aPixel^.alpha]);
  end;


  case MouseActionInput of
    maiSelectingRect:
    begin
      // Removing previous Rect
      CurrSelection := SelectionZoomInput;
      CurrSelection.Offset(-sbxInputImage.HorzScrollBar.Position,
        -sbxInputImage.VertScrollBar.Position);
      sbxInputImage.Canvas.DrawFocusRect(CurrSelection);

      // Updating Selection Rect
      FSelectionInput.BottomRight := Point(ImgX + 1, ImgY + 1);

      StatusBar.Panels[2].Text :=
        Format('(%5d, %5d)-(%5d, %5d)', [SelectionInput.Left,
        SelectionInput.Top, SelectionInput.Right, SelectionInput.Bottom]);

      // Drawing new Rect
      CurrSelection := SelectionZoomInput;
      CurrSelection.Offset(-sbxInputImage.HorzScrollBar.Position,
        -sbxInputImage.VertScrollBar.Position);
      sbxInputImage.Canvas.DrawFocusRect(CurrSelection);
    end;

    maiPaintingPixel: RemovePixelInput(ImgX, ImgY);

    // maiPickingPaintColor: ;

    maiFillingColor:
    begin
      if ssRight in Shift then // Right: Removing while painting
      begin
        RemoveSameColorNeighboursInput(ImgX, ImgY,
          ColorToBGRA(bColorFillInput.ButtonColor,
          eOpacityFillInput.Value),
          eToleranceFillInput.Value);
        ActualInputImage.InvalidateBitmap;
        DrawImageInput;
      end; // Left: Wait until mouse up
    end;

    else
      ;
  end;
end;

procedure TfrmIconBorder.DoImageMouseUp(Button : TMouseButton;
  Shift : TShiftState; ImgX, ImgY : LongInt);
var
  CurrSelection : TRect;
  aPixel : PBGRAPixel;
  aTPixel : TBGRAPixel;
begin
  aPixel := nil;

  if InRange(ImgX, 0, ActualInputImage.Width - 1) and
    InRange(ImgY, 0, ActualInputImage.Height - 1) then
    aPixel := ActualInputImage.ScanLine[ImgY] + ImgX;

  case MouseActionInput of
    maiSelectingRect:
      case Button of
        mbLeft:
        begin
          // Removing previous Rect, because can be drawn outside pbxInputImage
          CurrSelection := SelectionZoomInput;
          CurrSelection.Offset(-sbxInputImage.HorzScrollBar.Position,
            -sbxInputImage.VertScrollBar.Position);
          sbxInputImage.Canvas.DrawFocusRect(CurrSelection);

          SelectionInput.NormalizeRect;

          if SelectionInput.isEmpty then
            StatusBar.Panels[2].Text := '';

          DrawImageInput;
          MouseActionInput := maiSelectRect;
        end;

        mbRight: ;
        mbMiddle: ;
        mbExtra1: ;
        mbExtra2: ;
        else
          ;
      end;

    maiPaintingPixel:
    begin
      ActualInputImage.InvalidateBitmap;
      DrawImageInput;
      MouseActionInput := maiPaintPixel;
    end;

    maiPickingPaintColor:
    begin
      if Assigned(aPixel) then
      begin
        if aPixel^.alpha <> 0 then
        begin
          bColorPaintInput.ButtonColor := aPixel^.ToColor;
          bColorFillInput.ButtonColor := aPixel^.ToColor;
        end
        else
        begin
          bColorPaintInput.ButtonColor := clBlack;
          bColorFillInput.ButtonColor := clBlack;
        end;
        eOpacityPaintInput.Value := aPixel^.alpha;
        eOpacityFillInput.Value := aPixel^.alpha;
      end
      else
      begin
        bColorPaintInput.ButtonColor := clBlack;
        eOpacityPaintInput.Value := 0;
      end;

      // TODO: Do this in a better way...
      case Button of
        mbRight: // Selecting color in Paint
        begin
          MouseActionInput := maiPaintPixel;
        end;
        mbMiddle: // Selecting color in Fill
        begin
          MouseActionInput := maiFillColor;
        end;
      end;
    end;

    maiFillingColor:
    begin
      case Button of
        mbLeft: // Left removing when button up
        begin
          if Assigned(aPixel) then
          begin
            RemoveSameColorNeighboursInput(ImgX, ImgY,
              ColorToBGRA(bColorFillInput.ButtonColor,
              eOpacityFillInput.Value),
              eToleranceFillInput.Value);
            ActualInputImage.InvalidateBitmap;
            DrawImageInput;
          end;
          MouseActionInput := maiFillColor;
        end;
        else
          ;
      end;
    end;

    maiReplacingColor:
    begin
      if Assigned(aPixel) then
      begin

        // We need to make a copy because actual pixel color is changed
        //   when ReplaceColor is called.
        aTPixel := aPixel^;

        if chkCopyReplaceToFill.Checked then
        begin
          bColorFillInput.ButtonColor := BGRAToColor(aTPixel);
          eOpacityFillInput.Value := aTPixel.alpha;
        end;

        ActualInputImage.ReplaceColor(aTPixel,
          ColorToBGRA(bColorReplaceInput.ButtonColor,
          eOpacityReplaceInput.Value));

        ActualInputImage.InvalidateBitmap;
        DrawImageInput;
      end;
      MouseActionInput := maiReplaceColor;
    end;

    else
      ;
  end;
end;

procedure TfrmIconBorder.DoImageMouseDown(Button : TMouseButton;
  Shift : TShiftState; ImgX, ImgY : LongInt);
begin
  case MouseActionInput of
    maiSelectRect:
      case Button of
        mbLeft:
        begin
          SelectionInput.Create(Point(ImgX, ImgY), 0, 0);

          StatusBar.Panels[1].Text :=
            Format('(%5d, %5d)-(%5d, %5d)',
            [SelectionInput.Left, SelectionInput.Top,
            SelectionInput.Right, SelectionInput.Bottom]);

          MouseActionInput := maiSelectingRect;
        end;
        mbRight: bSelectionTransparentInputClick(nil);
        mbMiddle: bCutSelectionInputClick(nil);
        else
          ;
      end;

    maiPaintPixel:
    begin
      case Button of
        mbLeft:
        begin
          RemovePixelInput(ImgX, ImgY);
          MouseActionInput := maiPaintingPixel;
        end;
        mbRight:
        begin
          MouseActionInput := maiPickingPaintColor;
        end
        else
          ;
      end;
    end;

    maiFillColor:
    begin
      // Wait to mouse up
      case Button of
        mbLeft, mbRight:
        begin
          MouseActionInput := maiFillingColor;
        end;
        mbMiddle:
        begin
          MouseActionInput := maiPickingPaintColor;
        end
        else
          ;
      end;
    end;

    maiReplaceColor:
    begin
      // Wait to mouse up
      case Button of
        mbLeft, mbRight:
        begin
          MouseActionInput := maiReplacingColor;
        end;
        else
          ;
      end;
    end

    else
      ;
  end;
end;

function TfrmIconBorder.SelectionZoomInput : TRect;
begin
  Result.Top := SelectionInput.Top * ZoomInput;
  Result.Left := SelectionInput.Left * ZoomInput;
  Result.Right := SelectionInput.Right * ZoomInput;
  Result.Bottom := SelectionInput.Bottom * ZoomInput;
  Result.NormalizeRect;
end;

procedure TfrmIconBorder.actAddFileExecute(Sender : TObject);
begin
  SetDlgInitialDir(OpenFilesDialog, EIBConfig.LastInFolder);

  if not OpenFilesDialog.Execute then
    Exit;

  if OpenFilesDialog.Files.Count > 0 then
    EIBConfig.LastInFolder := ExtractFileDir(OpenFilesDialog.Files[0]);

  FileList.Items.AddStrings(OpenFilesDialog.Files, False);
end;

procedure TfrmIconBorder.actAddFolderExecute(Sender : TObject);
var
  i : integer;
  slFiles : TStringList;
begin
  SetDlgInitialDir(SelectDirectoryDialog, EIBConfig.LastInFolder);

  if not SelectDirectoryDialog.Execute then
    Exit;

  if SelectDirectoryDialog.Files.Count > 0 then
    EIBConfig.LastInFolder := ExtractFileDir(SelectDirectoryDialog.Files[0]);

  i := 0;
  slFiles := TStringList.Create;
  while i < SelectDirectoryDialog.Files.Count do
  begin
    slFiles.Clear;
    slFiles.BeginUpdate;
    FindAllFiles(slFiles, SelectDirectoryDialog.Files[i], '*.png', True);
    FileList.Items.AddStrings(slFiles, False);
    slFiles.EndUpdate;
    Inc(i);
  end;
  slFiles.Free;
end;

procedure TfrmIconBorder.actClearListExecute(Sender : TObject);
begin
  FileList.Clear;
  FreeAndNil(FActualInputImage);
  DrawImageInput;
  FreeAndNil(FActualOutputImage);
  DrawImageOutput;
end;

procedure TfrmIconBorder.actDeleteFileExecute(Sender : TObject);
begin
  if FileList.ItemIndex < 0 then
    Exit;

  if not FileExists(FileList.Items[FileList.ItemIndex]) then
    Exit;

  if not DeleteFileUTF8(FileList.Items[FileList.ItemIndex]) then
    Exit;

  actRemoveItem.Execute;
end;

procedure TfrmIconBorder.actOpenInpDirExecute(Sender : TObject);
begin
  if FileList.ItemIndex < 0 then
    Exit;

  OpenDocument(ExtractFileDir(FileList.Items[FileList.ItemIndex]));
end;

procedure TfrmIconBorder.actOpenOutDirExecute(Sender : TObject);
begin
  if DirectoryExistsUTF8(eOutputFolder.Directory) then
    OpenDocument(eOutputFolder.Directory);
end;

procedure TfrmIconBorder.actRemoveItemExecute(Sender : TObject);
var
  i : integer;
begin
  i := FileList.ItemIndex;

  FileList.DeleteSelected;
  FreeAndNil(FActualInputImage);
  DrawImageInput;
  FreeAndNil(FActualOutputImage);
  DrawImageOutput;

  if i >= FileList.Count then i := FileList.Count - 1;

  if i >= 0 then
  begin
    FileList.ItemIndex := i;
    FileList.Click;
  end;
end;

procedure TfrmIconBorder.actReplaceFileExecute(Sender : TObject);
begin
  if (not Assigned(ActualInputImage)) or (FileList.ItemIndex = -1) then
    Exit;

  ActualInputImage.SaveToFileUTF8(FileList.Items[FileList.ItemIndex]);
end;

procedure TfrmIconBorder.actReplaceInpFileExecute(Sender : TObject);
var
  aFile : string;
begin
  if (not Assigned(ActualOutputImage)) or (FileList.ItemIndex = -1) then
    Exit;

  aFile := FileList.Items[FileList.ItemIndex];

  if not chkOverwriteOutput.Checked then
  begin
    // File already exists, so we add the new file to de list
    aFile := CHXCheckFileRename(aFile);
    FileList.Items.Add(aFile);
  end;

  ActualOutputImage.SaveToFileUTF8(aFile);

  FileList.Click;
end;

procedure TfrmIconBorder.actSaveOutputExecute(Sender : TObject);
var
  aFile : string;
begin
  if (not Assigned(ActualOutputImage)) or (FileList.ItemIndex = -1) then
    Exit;

  aFile := eOutputFolder.Directory;
  ForceDirectoriesUTF8(aFile);
  aFile := IncludeTrailingPathDelimiter(aFile) +
    ExtractFileName(FileList.Items[FileList.ItemIndex]);

  if not chkOverwriteOutput.Checked then
    aFile := CHXCheckFileRename(aFile);

  ActualOutputImage.SaveToFileUTF8(aFile);
end;

procedure TfrmIconBorder.bAutoZoomInputClick(Sender : TObject);
begin
  AutoZoomInput;
end;

procedure TfrmIconBorder.bAutoZoomOutputClick(Sender : TObject);
begin
  AutoZoomOutput;
end;

procedure TfrmIconBorder.bBlackIconBorderClick(Sender : TObject);
begin
  bColorBorderEmutecaIcon.Enabled := True;
  bColorBorderEmutecaIcon.ButtonColor := clBlack;
  eOpacityBorderEmutecaIcon.Value := 255;
end;

procedure TfrmIconBorder.bCutSelectionInputClick(Sender : TObject);
begin
  if SelectionInput.isEmpty then
    Exit;

  BGRAReplace(FActualInputImage, ActualInputImage.GetPart(SelectionInput));

  SelectionInput := SelectionInput.Empty;
  StatusBar.Panels[2].Text := '';

  AutoZoomInput;
end;

procedure TfrmIconBorder.bDefaultIconBorderClick(Sender : TObject);
begin
  bColorBorderEmutecaIcon.Enabled := True;
  bColorBorderEmutecaIcon.ButtonColor := clGray;
  eOpacityBorderEmutecaIcon.Value := 128;
end;

procedure TfrmIconBorder.bFlipHInputClick(Sender : TObject);
begin
  ActualInputImage.HorizontalFlip;
  DrawImageInput;
end;

procedure TfrmIconBorder.bFlipVInputClick(Sender : TObject);
begin
  ActualInputImage.VerticalFlip;
  DrawImageInput;
end;

procedure TfrmIconBorder.bProcessOutputClick(Sender : TObject);

  procedure AutoCropTransparentOutput;

    function AutoCropTransparentFirstRow : boolean;
    var
      aColor : PBGRAPixel;
      i : integer;
    begin
      Result := False;
      if ActualOutputImage.Height <= 1 then
        Exit;

      aColor := ActualOutputImage.ScanLine[0];
      i := ActualOutputImage.Width;

      repeat
        Result := aColor^.alpha = 0;
        Inc(aColor);
        Dec(i);
      until (not Result) or (i <= 0);

      if Result then
      begin
        YOffset := YOffset + 1;
        BGRAReplace(FActualOutputImage, ActualOutputImage.GetPart(
          Rect(0, 1, ActualOutputImage.Width, ActualOutputImage.Height)));
      end;
    end;

    function AutoCropTransparentLastRow : boolean;
    var
      aColor : PBGRAPixel;
      i : integer;
    begin
      Result := False;
      if ActualOutputImage.Height <= 1 then
        Exit;

      aColor := ActualOutputImage.ScanLine[ActualOutputImage.Height - 1];
      i := ActualOutputImage.Width - 1;

      repeat
        Result := aColor^.alpha = 0;
        Inc(aColor);
        Dec(i);
      until (not Result) or (i < 0);

      if Result then
        BGRAReplace(FActualOutputImage, ActualOutputImage.GetPart(
          Rect(0, 0, ActualOutputImage.Width, ActualOutputImage.Height - 1)));
    end;

    function AutoCropTransparentFirstCol : boolean;
    var
      i : integer;
      aColor : PBGRAPixel;
    begin
      Result := False;
      if ActualOutputImage.Width <= 1 then
        Exit;

      i := ActualOutputImage.Height - 1;
      repeat
        aColor := ActualOutputImage.ScanLine[i];
        Result := aColor^.Alpha = 0;
        Dec(i);
      until (not Result) or (i < 0);

      if Result then
      begin
        XOffset := XOffset + 1;
        BGRAReplace(FActualOutputImage, ActualOutputImage.GetPart(
          Rect(1, 0, ActualOutputImage.Width, ActualOutputImage.Height)));

      end;
    end;

    function AutoCropTransparentLastCol : boolean;
    var
      i : integer;
      aColor : PBGRAPixel;
    begin
      Result := False;
      if ActualOutputImage.Width <= 1 then
        Exit;

      i := ActualOutputImage.Height - 1;
      repeat
        aColor := ActualOutputImage.ScanLine[i];
        Inc(aColor, ActualOutputImage.Width - 1);
        Result := aColor^.Alpha = 0;
        Dec(i);
      until (not Result) or (i < 0);

      if Result then
        BGRAReplace(FActualOutputImage, ActualOutputImage.GetPart(
          Rect(0, 0, ActualOutputImage.Width - 1, ActualOutputImage.Height)));
    end;

  var
    Cont : boolean;
  begin
    repeat
      Cont := AutoCropTransparentFirstRow;
      Cont := Cont or AutoCropTransparentLastRow;
      Cont := Cont or AutoCropTransparentFirstCol;
      Cont := Cont or AutoCropTransparentLastCol;
    until (not Cont);
  end;

  procedure RemoveSemitransparentPixels;
  var
    aColor : PBGRAPixel;
    i : integer;
  begin
    aColor := ActualOutputImage.Data;
    for i := 1 to ActualOutputImage.NbPixels do
    begin
      if aColor^.alpha < 255 then
        aColor^.Alpha := 0;
      Inc(aColor);
    end;
  end;

  procedure OpaqueSemitransparentPixels;
  var
    aColor : PBGRAPixel;
    i : integer;
  begin
    aColor := ActualOutputImage.Data;
    for i := 1 to ActualOutputImage.NbPixels do
    begin
      if aColor^.alpha > 0 then
        aColor^.Alpha := 255;
      Inc(aColor);
    end;
  end;

  procedure AutoReduceOutput;
  var
    x, y, i : integer;
    Factor : integer;
    CurrColor : PBGRAPixel;
  begin

    Factor := Min(ActualOutputImage.Width, ActualOutputImage.Height);

    x := 0;
    while (x < ActualOutputImage.Width) and (Factor > 1) do
    begin
      CurrColor := ActualOutputImage.ScanLine[0] + x;

      i := 0;
      y := 0;
      while (y < ActualOutputImage.Height) and (Factor > 1) do
      begin
        if (ActualOutputImage.ScanLine[y] + x)^ = CurrColor^ then
        begin
          Inc(i);
        end
        else
        begin
          Factor := GCD(Factor, i);
          CurrColor := ActualOutputImage.ScanLine[y] + x;
          i := 1;
        end;

        Inc(y);
      end;
      Factor := GCD(Factor, i);

      Inc(x);
    end;

    if Factor <= 1 then
      Exit;

    y := 0;
    while (y < ActualOutputImage.Height) and (Factor > 1) do
    begin
      CurrColor := ActualOutputImage.ScanLine[y];

      i := 0;
      x := 0;
      while (x < ActualOutputImage.Width) and (Factor > 1) do
      begin
        if (ActualOutputImage.ScanLine[y] + x)^ = CurrColor^ then
        begin
          Inc(i);
        end
        else
        begin
          Factor := GCD(Factor, i);
          CurrColor := ActualOutputImage.ScanLine[y] + x;
          i := 1;
        end;

        Inc(x);
      end;
      Factor := GCD(Factor, i);

      Inc(y);
    end;

    if Factor <= 1 then
      Exit;

    BGRAReplace(FActualOutputImage, ActualOutputImage.Resample(
      ActualOutputImage.Width div Factor, ActualOutputImage.Height div
      Factor, rmSimpleStretch));

  end;

  procedure AddSemitransparentBorder;
  var
    aColor, TempColor : PBGRAPixel;
    TempImg : TBGRABitmap;
    i : integer;
  begin

    // Hack (1/2): If opacity is 255, it will fail. So we change it to 254,
    //   a then we will revert again to 255

    if eOpacityBorderEmutecaIcon.Value = 255 then
      eOpacityBorderEmutecaIcon.Value := 254;

    // Adding transparent border, and making sure that there are not solid
    //   pixel in the borders
    TempImg := TBGRABitmap.Create(ActualOutputImage.Width +
      2, ActualOutputImage.Height + 2, BGRA(0, 0, 0, 0));

    TempImg.PutImage(1, 1, ActualOutputImage, dmDrawWithTransparency);

    BGRAReplace(FActualOutputImage, TempImg);
    // Output image OffSet moved
    XOffset := XOffset - 1;
    YOffset := YOffset - 1;


    aColor := ActualOutputImage.Data;

    for i := 1 to ActualOutputImage.NbPixels do
    begin

      // Sombra izquierda
      if aColor^.Alpha = 0 then
      begin
        if i mod ActualOutputImage.Width <> 0 then
        begin
          TempColor := aColor + 1;
          if TempColor^.Alpha = 255 then
          begin
            aColor^ := ColorToBGRA(bColorBorderEmutecaIcon.ButtonColor,
              eOpacityBorderEmutecaIcon.Value);
          end;
        end;
      end;


      // Sombra superior (Win)
      if aColor^.Alpha = 0 then
      begin
        if i > ActualOutputImage.Width then
        begin
          TempColor := aColor - ActualOutputImage.Width;
          if TempColor^.Alpha = 255 then
          begin
            aColor^ := ColorToBGRA(bColorBorderEmutecaIcon.ButtonColor,
              eOpacityBorderEmutecaIcon.Value);
          end;
        end;
      end;

      // Sombra derecha
      if aColor^.Alpha = 0 then
      begin
        if i mod ActualOutputImage.Width <> 1 then
        begin
          TempColor := aColor - 1;
          if TempColor^.Alpha = 255 then
          begin
            aColor^ := ColorToBGRA(bColorBorderEmutecaIcon.ButtonColor,
              eOpacityBorderEmutecaIcon.Value);
          end;
        end;
      end;


      // Sombra inferior (Win)
      if aColor^.Alpha = 0 then
      begin
        if i + ActualOutputImage.Width <= ActualOutputImage.NbPixels then
        begin
          TempColor := aColor + ActualOutputImage.Width;
          if TempColor^.Alpha = 255 then
          begin
            aColor^ := ColorToBGRA(bColorBorderEmutecaIcon.ButtonColor,
              eOpacityBorderEmutecaIcon.Value);
          end;
        end;
      end;

      Inc(aColor);
    end;

    // Hack (2/2): Reverting again to 255
    if eOpacityBorderEmutecaIcon.Value = 254 then
    begin
      eOpacityBorderEmutecaIcon.Value := 255;

      aColor := ActualOutputImage.Data;
      for i := 1 to ActualOutputImage.NbPixels do
      begin
        if aColor^.alpha = 254 then
          aColor^.Alpha := 255;
        Inc(aColor);
      end;
    end;

  end;

begin
  FreeAndNil(FActualOutputImage);
  FreeAndNil(FVisibleOutputImage);

  if not Assigned(ActualInputImage) then
    Exit;

  FActualOutputImage := ActualInputImage.Duplicate;
  XOffset := 0;
  YOffset := 0;

  case ProcessOutputFilter of
    pofEmutecaIconBorder:
    begin
      if chkRemoveTransEmutecaIcon.Checked then
        RemoveSemitransparentPixels;
      AutoCropTransparentOutput;
      AutoReduceOutput;
      AddSemitransparentBorder;
      AutoCropTransparentOutput;
    end;

    pofRemoveIconBorder:
    begin
      case rgbRemoveTrans.ItemIndex of
        1:
        begin
          if chkAutoCropTransparency.Checked then
            AutoCropTransparentOutput;
          OpaqueSemitransparentPixels;
          AutoReduceOutput;
        end;
        else
        begin
          RemoveSemitransparentPixels;
          if chkAutoCropTransparency.Checked then
            AutoCropTransparentOutput;
          AutoReduceOutput;
        end;
      end;
    end;

    else
      ;
  end;

  ActualOutputImage.InvalidateBitmap;

  AutoZoomOutput;
end;

procedure TfrmIconBorder.bRotateCCWInputClick(Sender : TObject);
begin
  BGRAReplace(FActualInputImage, ActualInputImage.RotateCCW);
  DrawImageInput;
end;

procedure TfrmIconBorder.bRotateCWInputClick(Sender : TObject);
begin
  BGRAReplace(FActualInputImage, ActualInputImage.RotateCW);
  DrawImageInput;
end;

procedure TfrmIconBorder.bSelectionTransparentInputClick(Sender : TObject);
begin
  if SelectionInput.isEmpty then
    Exit;

  ActualInputImage.FillRect(SelectionInput, BGRA(0, 0, 0, 0));

  DrawImageInput;
end;

procedure TfrmIconBorder.bTransparentFillClick(Sender : TObject);
begin
  bColorFillInput.Enabled := False;
  bColorFillInput.ButtonColor := clBlack;
  eOpacityFillInput.Value := 0;
end;

procedure TfrmIconBorder.bTransparentReplaceClick(Sender : TObject);
begin
  bColorReplaceInput.Enabled := False;
  bColorReplaceInput.ButtonColor := clBlack;
  eOpacityReplaceInput.Value := 0;
end;

procedure TfrmIconBorder.bTransparentPaintClick(Sender : TObject);
begin
  bColorPaintInput.Enabled := False;
  bColorPaintInput.ButtonColor := clBlack;
  eOpacityPaintInput.Value := 0;
end;

procedure TfrmIconBorder.bZoom1xInputClick(Sender : TObject);
begin
  if not Assigned(ActualInputImage) then
    Exit;

  ZoomInput := 1;
end;

procedure TfrmIconBorder.bZoom1xOutputClick(Sender : TObject);
begin
  if not Assigned(ActualOutputImage) then
    Exit;

  ZoomOutput := 1;
end;

procedure TfrmIconBorder.bZoomInInputClick(Sender : TObject);
begin
  if not Assigned(ActualInputImage) then
    Exit;

  ZoomInput := ZoomInput * 2;
end;

procedure TfrmIconBorder.bZoomInOutputClick(Sender : TObject);
begin
  if not Assigned(ActualOutputImage) then
    Exit;

  ZoomOutput := ZoomOutput * 2;
end;

procedure TfrmIconBorder.bZoomOutInputClick(Sender : TObject);
begin
  if not Assigned(ActualInputImage) then
    Exit;

  ZoomInput := ZoomInput div 2;
end;

procedure TfrmIconBorder.bZoomOutOutputClick(Sender : TObject);
begin
  if not Assigned(ActualOutputImage) then
    Exit;

  ZoomOutput := ZoomOutput div 2;
end;

procedure TfrmIconBorder.cbxColorBackgroundChange(Sender : TObject);
begin
  DrawImageInput;
  DrawImageOutput;
end;

procedure TfrmIconBorder.eOpacityFillInputChange(Sender : TObject);
begin
  bColorFillInput.Enabled := eOpacityFillInput.Value > 0;
end;

procedure TfrmIconBorder.eOpacityPaintInputChange(Sender : TObject);
begin
  bColorPaintInput.Enabled := eOpacityPaintInput.Value > 0;
end;

procedure TfrmIconBorder.eOpacityReplaceInputChange(Sender : TObject);
begin
  bColorReplaceInput.Enabled := eOpacityReplaceInput.Value > 0;
end;

procedure TfrmIconBorder.eOutputFolderChange(Sender : TObject);
begin
  EIBConfig.LastOutFolder := eOutputFolder.Directory;
end;

procedure TfrmIconBorder.FormCloseQuery(Sender : TObject;
  var CanClose : boolean);
begin
  CanClose := True;

  EIBConfig.SaveToFile('', False);
  FreeAndNil(FEIBConfig);
end;

procedure TfrmIconBorder.FileListClick(Sender : TObject);
var
  aFile : string;
begin
  FreeAndNil(FActualInputImage);
  FreeAndNil(FVisibleInputImage);
  FreeAndNil(FActualOutputImage);
  FreeAndNil(FVisibleOutputImage);
  DrawImageOutput;

  SelectionInput := SelectionInput.Empty;

  // No item selected
  if FileList.ItemIndex < 0 then
  begin
    DrawImageInput;
    Exit;
  end;

  aFile := FileList.Items[FileList.ItemIndex];

  if not FileExistsUTF8(aFile) then
  begin
    ShowMessageFmt(rsFileNotFound, [aFile]);
    FileList.Items.Delete(FileList.ItemIndex);
    DrawImageInput;
    Exit;
  end;

  try
    FActualInputImage := TBGRABitmap.Create(aFile);
  except
    FreeAndNil(FActualInputImage);
    ShowMessageFmt(rsErrorLoadingFile, [aFile]);
    FileList.Items.Delete(FileList.ItemIndex);
    DrawImageInput;
    Exit;
  end;

  AutoZoomInput;
end;

procedure TfrmIconBorder.FormCreate(Sender : TObject);
var
  BaseFolder : string;
  TempStr : string;
begin
  // Title of application, usually it's autodeleted in .lpr file...
  Application.Title := Format(krsFmtApplicationTitle,
    [Application.Title, GetFileVersion]);

  // Changing current folder to parents exe folder.
  BaseFolder := ExtractFileDir(ExcludeTrailingPathDelimiter(ProgramDirectory));
  ChDir(BaseFolder);

  // Loading translation
  TempStr := IncludeTrailingPathDelimiter(BaseFolder) + krsLocaleFolder;
  if not DirectoryExistsUTF8(TempStr) then
    mkdir(TempStr);
  SetDefaultLang('', TempStr);

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings which can cause errors.
  StandardFormatSettings;

  // Windows Caption
  Caption := Format(krsFmtWindowCaption, [Application.Title, Caption]);

  // Loading GUI config
  FEIBConfig := cEIBConfig.Create(self);
  EIBConfig.DefaultFileName :=
    SetAsAbsoluteFile(krsEIBName + '.ini', BaseFolder);

  LoadGUIConfig(EIBConfig.DefaultFileName);
  EIBConfig.LoadFromFile('');

  // TODO: Move all logic code to a CHXFrame
  // CreateFrames;

  ZoomInput := 1;
  ZoomOutput := 1;

  pgcImageInput.PageIndex := 0;
  MouseActionInput := maiSelectRect;

  pgcImageOutput.PageIndex := 0;
  ProcessOutputFilter := pofEmutecaIconBorder;
  eOutputFolder.Directory :=
    ExcludeTrailingPathDelimiter(SysPath(EIBConfig.LastOutFolder));
  SelectDirectoryDialog.InitialDir :=
    ExcludeTrailingPathDelimiter(SysPath(EIBConfig.LastInFolder));
  OpenFilesDialog.InitialDir :=
    ExcludeTrailingPathDelimiter(SysPath(EIBConfig.LastInFolder));

  // Reading commandline parameters
  if ParamCount > 0 then
  begin
    // First parameter OutFolder
    if ParamStr(1) <> '' then
      eOutputFolder.Directory := ParamStr(1);
    // Second parameter InFile
    if ParamStr(2) <> '' then
      FileList.Items.Add(ParamStr(2));
    // We don't want change EIBConfig on Exit
    EIBConfig.DefaultFileName := '';
  end;
end;

procedure TfrmIconBorder.FormDestroy(Sender : TObject);
begin
  ActualInputImage.Free;
  VisibleInputImage.Free;
  ActualOutputImage.Free;
  VisibleOutputImage.Free;
end;

procedure TfrmIconBorder.pbxInputImageMouseDown(Sender : TObject;
  Button : TMouseButton; Shift : TShiftState; X, Y : integer);
begin
  DoImageMouseDown(Button, Shift, X div ZoomInput, Y div ZoomInput);
end;

procedure TfrmIconBorder.pbxInputImageMouseLeave(Sender : TObject);
begin
  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := '';
end;

procedure TfrmIconBorder.pbxInputImageMouseMove(Sender : TObject;
  Shift : TShiftState; X, Y : integer);
begin
  DoImageMouseMove(Shift, X div ZoomInput, Y div ZoomInput);
end;

procedure TfrmIconBorder.pbxInputImageMouseUp(Sender : TObject;
  Button : TMouseButton; Shift : TShiftState; X, Y : integer);
begin
  DoImageMouseUp(Button, Shift, X div ZoomInput, Y div ZoomInput);
end;

procedure TfrmIconBorder.pbxInputImagePaint(Sender : TObject);
begin
  if not Assigned(VisibleInputImage) then
    Exit;

  VisibleInputImage.Draw(pbxInputImage.Canvas, 0, 0, False);
end;

procedure TfrmIconBorder.pbxOutputImageMouseDown(Sender : TObject;
  Button : TMouseButton; Shift : TShiftState; X, Y : integer);
var
  ImgX, ImgY : LongInt;
begin
  ImgX := (X div ZoomOutput) + XOffset;
  ImgY := (Y div ZoomOutput) + YOffset;

  if (ImgX < 0) or (ImgY < 0) then
    Exit;

  DoImageMouseDown(Button, Shift, ImgX, ImgY);
end;

procedure TfrmIconBorder.pbxOutputImageMouseMove(Sender : TObject;
  Shift : TShiftState; X, Y : integer);
var
  ImgX, ImgY : LongInt;
begin
  ImgX := (X div ZoomOutput) + XOffset;
  ImgY := (Y div ZoomOutput) + YOffset;

  if (ImgX < 0) or (ImgY < 0) then
    Exit;

  DoImageMouseMove(Shift, ImgX, ImgY);
end;

procedure TfrmIconBorder.pbxOutputImageMouseUp(Sender : TObject;
  Button : TMouseButton; Shift : TShiftState; X, Y : integer);
var
  ImgX, ImgY : LongInt;
begin
  ImgX := (X div ZoomOutput) + XOffset;
  ImgY := (Y div ZoomOutput) + YOffset;

  if (ImgX < 0) or (ImgY < 0) then
    Exit;

  DoImageMouseUp(Button, Shift, ImgX, ImgY);
end;

procedure TfrmIconBorder.pbxOutputImagePaint(Sender : TObject);
begin
  if not Assigned(VisibleOutputImage) then
    Exit;

  VisibleOutputImage.Draw(pbxOutputImage.Canvas, 0, 0, False);
end;

procedure TfrmIconBorder.pgcImageInputChange(Sender : TObject);
begin
  case pgcImageInput.PageIndex of
    // 0: Common
    1: MouseActionInput := maiSelectRect;
    2: MouseActionInput := maiPaintPixel;
    3: MouseActionInput := maiFillColor;
    4: MouseActionInput := maiReplaceColor;
    else
      ;
  end;
end;

procedure TfrmIconBorder.pgcImageOutputChange(Sender : TObject);
begin
  case pgcImageOutput.PageIndex of
    0: ProcessOutputFilter := pofEmutecaIconBorder;
    1: ProcessOutputFilter := pofRemoveIconBorder;
    else
      ;
  end;
end;

procedure TfrmIconBorder.rgbBackGroundSelectionChanged(Sender : TObject);
begin
  // Enabling color button
  cbxColorBackground.Enabled := rgbBackGround.ItemIndex = 2;

  DrawImageInput;
  DrawImageOutput;
end;

end.
