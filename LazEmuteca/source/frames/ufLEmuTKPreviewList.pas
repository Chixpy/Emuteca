unit ufLEmuTKPreviewList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, ActnList,
  uCHXImageUtils,
  ufCHXFrame;

resourcestring
  rsTotalItemsCount = ' / %0:d';

type

  { TfmLEmuTKPreviewList }

  TfmLEmuTKPreviewList = class(TfmCHXFrame)
    actNextItem: TAction;
    actPreviousItem: TAction;
    alPreviewList: TActionList;
    cbxCurrItem: TComboBox;
    ilPreviewList: TImageList;
    lMaxItems: TLabel;
    pPreview: TPanel;
    tbNextItem: TToolButton;
    tbPreviewList: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton6: TToolButton;
    procedure actNextItemExecute(Sender: TObject);
    procedure actPreviousItemExecute(Sender: TObject);
    procedure cbxCurrItemSelect(Sender: TObject);

  private
    FCurrItem: integer;
    FItemCount: integer;
    procedure SetCurrItem(AValue: integer);
    procedure SetItemCount(AValue: integer);

  protected
    procedure SetGUIIconsIni(AValue: string); override;
    procedure OnCurrItemChange; virtual; abstract;

  public
    property ItemCount: integer read FItemCount write SetItemCount;
    property CurrItem: integer read FCurrItem write SetCurrItem;

    procedure ClearData; override;
    procedure LoadData; override;
    procedure SaveData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmLEmuTKPreviewList }

procedure TfmLEmuTKPreviewList.SetItemCount(AValue: integer);
begin
  if FItemCount = AValue then
    Exit;
  FItemCount := AValue;

  LoadData;
end;

procedure TfmLEmuTKPreviewList.SetGUIIconsIni(AValue: string);
begin
  inherited SetGUIIconsIni(AValue);
  ReadActionsIcons(GUIIconsIni, Name, ilPreviewList, alPreviewList);
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
  FCurrItem := AValue;

  if ItemCount > 0 then
  begin
    // Keep in range [1..ItemCount]
    if CurrItem > ItemCount then
    begin
      CurrItem := ItemCount;
    end
    else if CurrItem < 1 then
    begin
      CurrItem := 1;
    end;
  end
  else
  begin
    FCurrItem := 0;
  end;

  cbxCurrItem.ItemIndex := FCurrItem - 1;

  OnCurrItemChange;
end;

procedure TfmLEmuTKPreviewList.ClearData;
begin
  lMaxItems.Caption := format(rsTotalItemsCount, [ItemCount]);
  cbxCurrItem.Clear;
  CurrItem := 0;
end;

procedure TfmLEmuTKPreviewList.LoadData;
var
  i: integer;
begin
  ClearData;

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
  cbxCurrItem.Enabled := ItemCount > 1;

  CurrItem := 1;
end;

procedure TfmLEmuTKPreviewList.SaveData;
begin

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
