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
    actFirstItem: TAction;
    actOpenItem: TAction;
    actLastItem: TAction;
    actNextItem: TAction;
    actPreviousItem: TAction;
    alPreviewList: TActionList;
    esCurrItem: TSpinEdit;
    ilPreviewList: TImageList;
    lMaxItems: TLabel;
    tbPreviewList: TToolBar;
    ToolButton1: TToolButton;
    tbFirstItem: TToolButton;
    ToolButton3: TToolButton;
    tbNextItem: TToolButton;
    tbLastItem: TToolButton;
    ToolButton6: TToolButton;
    tbOpenItem: TToolButton;

  private
    FIconsIni: TFilename;
    procedure SetIconsIni(AValue: TFilename);

  public
    property IconsIni: TFilename read FIconsIni write SetIconsIni;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKPreviewList }

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
