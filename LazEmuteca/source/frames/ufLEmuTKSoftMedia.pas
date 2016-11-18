unit ufLEmuTKSoftMedia;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  ucEmutecaParent, ucEmutecaSoftware;

type

  { TfmLEmuTKSoftMedia }

  TfmLEmuTKSoftMedia = class(TFrame)
  private
    FGameGroup: cEmutecaParent;
    FSoftware: cEmutecaSoftware;
    procedure SetGameGroup(AValue: cEmutecaParent);
    procedure SetSoftware(AValue: cEmutecaSoftware);
    { private declarations }
  public
    { public declarations }

    property GameGroup: cEmutecaParent read FGameGroup write SetGameGroup;
    property Software: cEmutecaSoftware read FSoftware write SetSoftware;

                  constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKSoftMedia }

procedure TfmLEmuTKSoftMedia.SetGameGroup(AValue: cEmutecaParent);
begin
  if FGameGroup = AValue then Exit;
  FGameGroup := AValue;
end;

procedure TfmLEmuTKSoftMedia.SetSoftware(AValue: cEmutecaSoftware);
begin
  if FSoftware = AValue then Exit;
  FSoftware := AValue;
end;

constructor TfmLEmuTKSoftMedia.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmLEmuTKSoftMedia.Destroy;
begin
  inherited Destroy;
end;

end.

