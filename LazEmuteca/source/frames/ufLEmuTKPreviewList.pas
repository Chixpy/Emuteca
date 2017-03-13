unit ufLEmuTKPreviewList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, ActnList,
  Buttons, ExtCtrls,
  uCHXStrUtils, uCHXImageUtils;

type

  { TfmLEmuTKPreviewList }

  TfmLEmuTKPreviewList = class(TFrame)
    actNextItem: TAction;
    actPreviousItem: TAction;
    alPreviewList: TActionList;
    cbxCurrItem: TComboBox;
    ilPreviewList: TImageList;
    lMaxItems: TLabel;
    pPreview: TPanel;
    tbPreviewList: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    tbNextItem: TToolButton;
    ToolButton6: TToolButton;
    procedure actNextItemExecute(Sender: TObject);
    procedure actPreviousItemExecute(Sender: TObject);
    procedure cbxCurrItemSelect(Sender: TObject);

  private
    FCurrItem: integer;
    FIconsIni: string;
    FItemCount: integer;
    procedure SetCurrItem(AValue: integer);
    procedure SetItemCount(AValue: integer);

  protected
    property ItemCount: integer read FItemCount write SetItemCount default 0;
    property CurrItem: integer read FCurrItem write SetCurrItem default 0;

    procedure SetIconsIni(AValue: string); virtual;
    procedure OnCurrItemChange; virtual; abstract;

  public
    property IconsIni: string read FIconsIni write SetIconsIni;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKPreviewList }

procedure TfmLEmuTKPreviewList.SetItemCount(AValue: integer);
var
  i: Integer;
begin
  if FItemCount = AValue then Exit;
  FItemCount := AValue;

  lMaxItems.Caption := ' / ' + IntToStr(ItemCount);

  cbxCurrItem.Items.BeginUpdate;
  try
  cbxCurrItem.Items.Clear;
  for i := 1 to ItemCount do
    cbxCurrItem.Items.Add(IntToStr(i));
    cbxCurrItem.Enabled := ItemCount > 1;
  finally
    cbxCurrItem.Items.EndUpdate;
  end;

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

procedure TfmLEmuTKPreviewList.cbxCurrItemSelect(Sender: TObject);
begin
  CurrItem := cbxCurrItem.ItemIndex + 1;
end;

procedure TfmLEmuTKPreviewList.SetCurrItem(AValue: integer);
begin
  // Update anyway
  // if FCurrItem = AValue then Exit;
  FCurrItem := AValue;

  cbxCurrItem.ItemIndex := FCurrItem - 1;

  OnCurrItemChange;
end;

procedure TfmLEmuTKPreviewList.SetIconsIni(AValue: string);
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
