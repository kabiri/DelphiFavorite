{
  Unit Name   : WP.Favorite.New.pas
  Author      : Hamid Reza Kabiri
  Created     : 04/09/2025
  Description : Create a new list

  History:
    -

  Notes:
}

unit WP.Favorite.New;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmNewFavorite = class(TForm)
    edtFavoriteName: TEdit;
    lblFavoriteName: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    btnSave: TButton;
    btnCancel: TButton;
    btnAdd: TButton;
    btnDel: TButton;
    pnlAvailable: TPanel;
    sbAvailable: TScrollBox;
    pnlSelected: TPanel;
    sbSelected: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    AvailableProjects : TStringList;
    SelectedProjects : TStringList;
    Procedure ShowAvailableProjects;
    Procedure ShowSelectedProjects;
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ProjectClick(Sender: TObject);
    procedure ProjectDblClick(Sender: TObject);
    procedure AddPanel(owner : TScrollBox; FileName : string);
    { Private declarations }
  public
    FavoriteId : Integer;
    procedure GetProjectList;
    { Public declarations }
  end;

var
  frmNewFavorite: TfrmNewFavorite;

implementation

uses
  WP.Favorite.Utils
  , System.StrUtils;

{$R *.dfm}

procedure TfrmNewFavorite.btnAddClick(Sender: TObject);
var
  I , J : Integer;
begin
  for var Item in sbAvailable.GetControls do
  begin
    if (Item is TPanel) then
    begin
      var pnl:=(Item as TPanel);
      for J := 0 to pnl.ComponentCount-1 do
      begin
        if (pnl.Components[J] is TCheckBox) then
        begin
          if (pnl.Components[J] as TCheckBox).Checked then
          begin
            AddPanel(sbSelected,pnl.Caption);
            pnl.Destroy;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmNewFavorite.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmNewFavorite.btnDelClick(Sender: TObject);
var
  I , J : Integer;
begin
  for var Item in sbSelected.GetControls do
  begin
    if (Item is TPanel) then
    begin
      var pnl:=(Item as TPanel);
      for J := 0 to pnl.ComponentCount-1 do
      begin
        if (pnl.Components[J] is TCheckBox) then
        begin
          if (pnl.Components[J] as TCheckBox).Checked then
          begin
            AddPanel(sbAvailable,pnl.Caption);
            pnl.Destroy;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmNewFavorite.btnSaveClick(Sender: TObject);
begin
  edtFavoriteName.Text:=Trim(edtFavoriteName.Text);
  if edtFavoriteName.Text='' then
  begin
    ShowMessage('Please fill favorite name.');
    edtFavoriteName.SetFocus;
    Exit;
  end;

  if sbSelected.ComponentCount=0 then
  begin
    ShowMessage('Please add project.');
    Exit;
  end;

  var Selected:=TStringList.Create;
  try
    for var Item in sbSelected.GetControls do
    begin
      if (Item is TPanel) then
      begin
        var pnl:=(Item as TPanel);
        Selected.Add(pnl.Caption);
      end;
    end;

    if Selected.Count=0 then
    begin
      ShowMessage('Please add project.');
      Selected.Free;
      Exit;
    end;

    if FavoriteId=-1 then
    begin
      var lastId:=SaveFavorite(edtFavoriteName.Text);
      if Selected.Count>0 then
        SaveToFavorite(lastId.ToString,Selected);
    end
    else
    begin
      EditFavorite(FavoriteId.ToString,edtFavoriteName.Text);
      SaveToFavorite(FavoriteId.ToString,Selected);
    end;
  finally
    Selected.Free;
  end;

  Close;
end;

procedure TfrmNewFavorite.FormCreate(Sender: TObject);
begin
  AvailableProjects:=TStringList.Create;
  SelectedProjects:=TStringList.Create;
end;

procedure TfrmNewFavorite.FormDestroy(Sender: TObject);
begin
  SelectedProjects.Free;
  AvailableProjects.Free;
end;

procedure TfrmNewFavorite.FormShow(Sender: TObject);
begin
  GetRecentProjects(AvailableProjects);
  if FavoriteId<>-1 then
    GetProjectList;
  ShowAvailableProjects;
  if FavoriteId<>-1 then
    ShowSelectedProjects;
end;

procedure TfrmNewFavorite.GetProjectList;
var
  prList : TStringList;
begin
  if FavoriteId>-1 then
  begin
    prList:=TStringList.Create;
    LoadFromFavorite(FavoriteId.ToString,prList);
    for var I := 0 to prList.Count-1 do
    begin
      SelectedProjects.Add(prList[I]);
      for var J := 0 to AvailableProjects.Count-1 do
      begin
        if AvailableProjects[J]=prList[I] then
        begin
          AvailableProjects.Delete(J);
          Break;
        end;
      end;
    end;
    prList.Free;
  end;
end;

procedure TfrmNewFavorite.ShowAvailableProjects;
var
  lbl : TLabel;
  pnl , pnlc : TPanel;
  chk : TCheckBox;
  find : Boolean;
begin
  for var I := 0 to AvailableProjects.Count-1 do
  begin
    find:=False;
    if FavoriteId<>-1 then
    begin
      for var J := 0 to SelectedProjects.Count-1 do
      begin
        if AvailableProjects[I]=SelectedProjects[J] then
        begin
          find:=True;
          Break;
        end;
      end;
    end;
    if not find then
      AddPanel(sbAvailable,AvailableProjects[I]);
  end;

  sbAvailable.VertScrollBar.Increment:=40;
  sbAvailable.Tag:=40;
  sbAvailable.OnMouseWheel:=MouseWheel;
end;

procedure TfrmNewFavorite.ProjectClick(Sender: TObject);
var
  pnl : TPanel;
begin
  pnl:=(Sender as TPanel);

  if pnl.BevelOuter=bvLowered then
    pnl.BevelOuter:=bvRaised
  else
    pnl.BevelOuter:=bvLowered;
end;

procedure TfrmNewFavorite.ProjectDblClick(Sender: TObject);
var
  pnl : TPanel;
begin
  pnl:=(Sender as TPanel);

  if pnl.Parent=sbAvailable then
    pnl.Parent:=sbSelected
  else
    pnl.Parent:=sbAvailable;
end;

procedure TfrmNewFavorite.ShowSelectedProjects;
begin
  for var I := 0 to SelectedProjects.Count-1 do
  begin
    AddPanel(sbSelected,SelectedProjects[I]);
  end;

  sbSelected.VertScrollBar.Increment:=40;
  sbSelected.Tag:=40;
  sbSelected.OnMouseWheel:=MouseWheel;
end;

procedure TfrmNewFavorite.AddPanel(owner : TScrollBox; FileName : string);
var
  lbl : TLabel;
  pnl , pnlc : TPanel;
  chk : TCheckBox;
begin
  pnl:=TPanel.Create(owner);
  pnl.BevelOuter:=bvLowered;
  pnl.Top:=999999;
  pnl.Align:=alTop;
  pnl.Height:=40;
  pnl.ShowCaption:=False;
  pnl.Caption:=FileName;
  pnl.Margins.Top:=1;
  pnl.Margins.Right:=2;
  pnl.Margins.Bottom:=2;
  pnl.Margins.Left:=2;
  pnl.AlignWithMargins:=True;
  pnl.ParentColor:=False;
  pnl.ParentBackground:=False;
  pnl.Parent:=owner;
  pnl.OnDblClick:=ProjectDblClick;
  pnl.Cursor:=crHandPoint;

  chk:=TCheckBox.Create(pnl);
  chk.Align:=alLeft;
  chk.Caption:='';
  chk.Width:=17;
  chk.Parent:=pnl;

  pnlc:=TPanel.Create(pnl);
  pnlc.BevelOuter:=bvLowered;
  pnlc.Align:=alClient;
  pnlc.Parent:=pnl;
  pnlc.Enabled := False;

  lbl:=TLabel.Create(pnlc);
  lbl.Caption:=ExtractFileName(FileName);
  lbl.Parent:=pnlc;
  lbl.Top:=999999;
  lbl.Hint:=FileName;
  lbl.Align:=alTop;
  lbl.Font.Style:=[TFontStyle.fsBold];
  lbl.AutoSize:=True;
  lbl.WordWrap:=True;
  lbl.Margins.Top:=3;
  lbl.Margins.Right:=3;
  lbl.Margins.Bottom:=0;
  lbl.Margins.Left:=5;
  lbl.AlignWithMargins:=True;

  lbl:=TLabel.Create(pnlc);
  lbl.Caption:=ExtractFilePath(FileName);
  lbl.Parent:=pnlc;
  lbl.Top:=999999;
  lbl.Align:=alTop;
  lbl.AutoSize:=True;
  lbl.WordWrap:=True;
  lbl.Margins.Top:=3;
  lbl.Margins.Right:=3;
  lbl.Margins.Bottom:=0;
  lbl.Margins.Left:=10;
  lbl.AlignWithMargins:=True;
end;

procedure TfrmNewFavorite.MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    (Sender as TScrollBox).VertScrollBar.Position := (Sender as TScrollBox).VertScrollBar.Position - (Sender as TScrollBox).Tag
  else
    (Sender as TScrollBox).VertScrollBar.Position := (Sender as TScrollBox).VertScrollBar.Position + (Sender as TScrollBox).Tag;
  Handled := True;
end;

end.
