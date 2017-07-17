unit ufLEmuTKSoftTxtPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ActnList,
  ucEmuteca, ucEmutecaSystem, ucEmutecaGroup, ucEmutecaSoftware,
  ufLEmuTKPreviewList, uGUIConfig;

type

  { TfmLEmuTKSoftTxtPreview }

  TfmLEmuTKSoftTxtPreview = class(TfmLEmuTKPreviewList)
    cbxTextType: TComboBox;
    mSoftText: TMemo;
    procedure cbxTextTypeSelect(Sender: TObject);

  private
    FCurrCaption: string;
    FCurrSystem: cEmutecaSystem;
    FEmuteca: cEmuteca;
    FGroup: cEmutecaGroup;
    FGUIConfig: cGUIConfig;
    FTxtExt: TStringList;
    FTxtList: TStringList;
    FSoftware: cEmutecaSoftware;
    procedure SetCurrCaption(AValue: string);
    procedure SetCurrSystem(AValue: cEmutecaSystem);
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetGroup(AValue: cEmutecaGroup);
    procedure SetGUIConfig(AValue: cGUIConfig);
    procedure SetTxtExt(AValue: TStringList);
    procedure SetSoftware(AValue: cEmutecaSoftware);

  protected
    property CurrSystem: cEmutecaSystem read FCurrSystem write SetCurrSystem;
    property CurrCaption: string read FCurrCaption write SetCurrCaption;
    property TxtList: TStringList read FTxtList;

    procedure OnCurrItemChange; override;
    procedure UpdateTxtList;

  public
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;
    property Group: cEmutecaGroup read FGroup write SetGroup;
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
        property GUIConfig: cGUIConfig read FGUIConfig write SetGUIConfig;

    property TxtExt: TStringList read FTxtExt write SetTxtExt;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSoftTxtPreview }

procedure TfmLEmuTKSoftTxtPreview.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then
    Exit;
  FSoftware := AValue;

  //if not Assigned(Software) then
  //  CurrSystem := nil
  //else
  //  CurrSystem := Software.CachedSystem;

  UpdateTxtList;

  Enabled := Assigned(Software);
end;

procedure TfmLEmuTKSoftTxtPreview.OnCurrItemChange;
begin
  if (cbxTextType.ItemIndex = -1) or (CurrItem < 1) or
    (TxtList.Count = 0) then
  begin
    mSoftText.Clear;
    exit;
  end;

  mSoftText.Lines.LoadFromFile(TxtList[CurrItem - 1]);
end;

procedure TfmLEmuTKSoftTxtPreview.UpdateTxtList;
begin
  TxtList.Clear;
  mSoftText.Clear;

  if not assigned(CurrSystem) then
    Exit;

  if not Assigned(Emuteca) then
    Exit;

  //if cbxTextType.ItemIndex > -1 then
  //  Emuteca.SearchSoftFiles(TxtList,
  //    Software.CachedSystem.TextFolders[cbxTextType.ItemIndex],
  //    Software, TxtExt);

  ItemCount := TxtList.Count;

  if ItemCount > 0 then
    CurrItem := 1
  else
    CurrItem := 0;
end;

procedure TfmLEmuTKSoftTxtPreview.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;
end;

procedure TfmLEmuTKSoftTxtPreview.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup = AValue then
    Exit;
  FGroup := AValue;

  //if not Assigned(Group) then
  //  CurrSystem := nil
  //else
  //  CurrSystem := Group.System;

  UpdateTxtList;

  Enabled := Assigned(Group);
end;

procedure TfmLEmuTKSoftTxtPreview.SetGUIConfig(AValue: cGUIConfig);
begin
  if FGUIConfig = AValue then Exit;
  FGUIConfig := AValue;
end;

procedure TfmLEmuTKSoftTxtPreview.cbxTextTypeSelect(Sender: TObject);
var
  aIndex: integer;
begin
  aIndex := cbxTextType.ItemIndex;
  if aIndex = -1 then
    CurrCaption := ''
  else
    CurrCaption := cbxTextType.Items[cbxTextType.ItemIndex];

  UpdateTxtList;
end;

procedure TfmLEmuTKSoftTxtPreview.SetCurrCaption(AValue: string);
begin
  if FCurrCaption = AValue then
    Exit;
  FCurrCaption := AValue;
end;

procedure TfmLEmuTKSoftTxtPreview.SetCurrSystem(AValue: cEmutecaSystem);
var
  aIndex: integer;
begin
  if FCurrSystem = AValue then
    Exit;
  FCurrSystem := AValue;

  if Assigned(CurrSystem) then
  begin
    // Updating captions
    cbxTextType.Items.Assign(CurrSystem.TextCaptions);

    // Selecting previous one if exists
    aIndex := cbxTextType.Items.IndexOf(CurrCaption);
    if aIndex <> -1 then
    begin
      cbxTextType.ItemIndex := aIndex;
    end
    else
    begin
      if cbxTextType.Items.Count > 0 then
      begin
        cbxTextType.ItemIndex := 0;
        // CurrCaption := ''; Not clearing this maybe is interenting...
      end;
    end;
  end
  else
  begin
    cbxTextType.Clear;
    // CurrCaption := ''; Keep this...
  end;
end;

procedure TfmLEmuTKSoftTxtPreview.SetTxtExt(AValue: TStringList);
begin
  if FTxtExt = AValue then
    Exit;
  FTxtExt := AValue;
end;

constructor TfmLEmuTKSoftTxtPreview.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FTxtList := TStringList.Create;
end;

destructor TfmLEmuTKSoftTxtPreview.Destroy;
begin
  FreeAndNil(FTxtList);
  inherited Destroy;
end;

end.
