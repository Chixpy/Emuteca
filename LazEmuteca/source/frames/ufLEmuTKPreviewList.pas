unit ufLEmuTKPreviewList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, ActnList,
  Spin, Buttons, ExtCtrls,
  uCHXStrUtils, uCHXImageUtils;

type

  { TfmLEmuTKPreviewList }

  TfmLEmuTKPreviewList = class(TFrame)
    actNextItem: TAction;
    actPreviousItem: TAction;
    alPreviewList: TActionList;
    esCurrItem: TSpinEdit;
    ilPreviewList: TImageList;
    lMaxItems: TLabel;
    tbPreviewList: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    tbNextItem: TToolButton;
    ToolButton6: TToolButton;
    procedure actNextItemExecute(Sender: TObject);
    procedure actPreviousItemExecute(Sender: TObject);

  private
    FCurrItem: integer;
    FIconsIni: TFilename;
    FItemCount: integer;
    procedure SetCurrItem(AValue: integer);
    procedure SetItemCount(AValue: integer);

  protected
    property ItemCount: integer read FItemCount write SetItemCount default 0;
    property CurrItem: integer read FCurrItem write SetCurrItem default 0;

    procedure SetIconsIni(AValue: TFilename); virtual;
    procedure OnCurrItemChange; virtual; abstract;

  public
    property IconsIni: TFilename read FIconsIni write SetIconsIni;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKPreviewList }

procedure TfmLEmuTKPreviewList.SetItemCount(AValue: integer);
begin
  if FItemCount = AValue then Exit;
  FItemCount := AValue;

  lMaxItems.Caption := ' / ' + IntToStr(ItemCount);
  esCurrItem.MaxValue := ItemCount;
  esCurrItem.Enabled := ItemCount > 1;
  actNextItem.Enabled := ItemCount > 1;
  actPreviousItem.Enabled := ItemCount > 1;
end;

procedure TfmLEmuTKPreviewList.actNextItemExecute(Sender: TObject);
begin
  if ItemCount < 1 then
   Exit;
  if CurrItem = ItemCount then
   CurrItem := 1
  else
    CurrItem := CurrItem + 1;
end;

procedure TfmLEmuTKPreviewList.actPreviousItemExecute(Sender: TObject);
begin
  if ItemCount < 1 then
   Exit;
  if CurrItem = 1 then
   CurrItem := ItemCount
  else
    CurrItem := CurrItem - 1;
end;

procedure TfmLEmuTKPreviewList.SetCurrItem(AValue: integer);
begin
  // Update anyway
  // if FCurrItem = AValue then Exit;
  FCurrItem := AValue;

  esCurrItem.Value := FCurrItem;

  OnCurrItemChange;
end;

procedure TfmLEmuTKPreviewList.SetIconsIni(AValue: TFilename);
begin
  FIconsIni := SetAsFile(AValue);
  ReadActionsIcons(IconsIni, Self.Name, ilPreviewList, alPreviewList);
end;

constructor TfmLEmuTKPreviewList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmLEmuTKPreviewList.Destroy;
begin
  inherited Destroy;
end;

end.
