unit ufLEmuTKPreviewList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, ActnList,
  uCHXImageUtils;

type

  { TfmLEmuTKPreviewList }

  TfmLEmuTKPreviewList = class(TFrame)
    alPreviewList: TActionList;
    ilPreviewList: TImageList;
    tbPreviewList: TToolBar;
    tbOpen: TToolButton;
    ToolButton2: TToolButton;
  private
    FIconsIni: string;
    procedure SetIconsIni(AValue: string);

  public
    property IconsIni: string read FIconsIni write SetIconsIni;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKPreviewList }

procedure TfmLEmuTKPreviewList.SetIconsIni(AValue: string);
begin
  if FIconsIni = AValue then
    Exit;
  FIconsIni := AValue;

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
