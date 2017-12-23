unit ufLEmuTKactMergeGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ufrCHXForm,
  ufCHXPropEditor,
  uEmutecaCommon,
  ucEmutecaGroup;

type

  { TfmLEmuTKactMergeGroup }

  TfmLEmuTKactMergeGroup = class(TfmCHXPropEditor)
  private
    FGroup: cEmutecaGroup;
    procedure SetGroup(AValue: cEmutecaGroup);

  protected
        procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property Group: cEmutecaGroup read FGroup write SetGroup;

    // Creates a form with AddSoft frame.
    class function SimpleForm(aGroup: cEmutecaGroup; aGUIIconsIni: string;
      aGUIConfigIni: string): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKactMergeGroup }

procedure TfmLEmuTKactMergeGroup.SetGroup(AValue: cEmutecaGroup);
begin
  if FGroup=AValue then Exit;
  FGroup:=AValue;
end;

procedure TfmLEmuTKactMergeGroup.DoClearFrameData;
begin

end;

procedure TfmLEmuTKactMergeGroup.DoLoadFrameData;
begin

end;

procedure TfmLEmuTKactMergeGroup.DoSaveFrameData;
begin

end;

class function TfmLEmuTKactMergeGroup.SimpleForm(aGroup: cEmutecaGroup;
  aGUIIconsIni: string; aGUIConfigIni: string): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmLEmuTKactMergeGroup;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmLEmuTKactMergeGroup';
    aForm.Caption := Format(krsFmtWindowCaption,
      [Application.Title, 'Merge group files...']);
    aForm.AutoSize := True;

    aFrame := TfmLEmuTKactMergeGroup.Create(aForm);
    aFrame.SaveButtons := True;
    aFrame.ButtonClose := True;
    aFrame.Align := alClient;

    aFrame.Group := aGroup;

    aForm.LoadGUIConfig(aGUIConfigIni);
    aForm.LoadGUIIcons(aGUIIconsIni);
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

constructor TfmLEmuTKactMergeGroup.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Enabled := False;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmLEmuTKactMergeGroup.Destroy;
begin
  inherited Destroy;
end;

end.

