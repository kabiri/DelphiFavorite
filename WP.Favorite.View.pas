{
  Unit Name   : WP.Favorite.View.pas
  Author      : Hamid Reza Kabiri
  Created     : 04/09/2025
  Description : Main page of the plugin

  History:
    -

  Notes:
}

unit WP.Favorite.View;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Generics.Collections, System.StrUtils, System.ImageList,
  Vcl.ImgList, Vcl.Buttons;

type
  TfrmMain = class(TFrame)
    pnlMenu: TPanel;
    btnNew: TButton;
    pnlList: TPanel;
    pnlProjects: TPanel;
    btnBack: TButton;
    btnEdit: TButton;
    lblFavoriteName: TLabel;
    imgl: TImageList;
    sbList: TScrollBox;
    sbProjects: TScrollBox;
    pnlRecent: TPanel;
    sbRecent: TScrollBox;
    btnRecent: TButton;
    procedure btnNewClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnRecentClick(Sender: TObject);
  private
    procedure loadFavoriteList;
    procedure loadProjectList(FavoriteId : Integer);
    procedure FavoriteClick(Sender: TObject);
    procedure DelFavoriteClick(Sender: TObject);
    procedure upClick(Sender: TObject);
    procedure downClick(Sender: TObject);
    procedure ProjectClick(Sender: TObject);
    procedure MouseEnter(Sender: TObject);
    procedure MouseLeave(Sender: TObject);
    procedure pnlMouseEnter(Sender: TObject);
    procedure pnlMouseLeave(Sender: TObject);
    procedure OpenDelphiProject(const AProjectPath: string);
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy;
    { Public declarations }
  end;

var
  MainFrame: TfrmMain;
  FavoriteList : TList<TPanel>;
  ProjectList : TList<TPanel>;

implementation

{$R *.dfm}

uses
  ToolsAPI,
  WP.Favorite.New,
  WP.Favorite.Setting,
  WP.Favorite.Utils;


procedure TfrmMain.OpenDelphiProject(const AProjectPath: string);
var
  ProjectGroup: IOTAProjectGroup;
begin
    (BorlandIDEServices as IOTAActionServices).OpenProject(AProjectPath,False);
end;

procedure TfrmMain.pnlMouseEnter(Sender: TObject);
var
  pnl , pnlL : TPanel;
  btn : TSpeedButton;
  btnU : TSpeedButton;
  btnD : TSpeedButton;
  I: Integer;
begin
  pnl:=(Sender as TLabel).Parent as TPanel;
  pnl.Color:=clBtnHighlight;
  for I := 0 to FavoriteList.Count-1 do
  begin
    pnlL:=FavoriteList[I];
    btn:=(pnlL.FindComponent('del'+pnlL.Tag.ToString) as TSpeedButton);
    btnU:=(pnlL.FindComponent('up'+pnlL.Tag.ToString) as TSpeedButton);
    btnD:=(pnlL.FindComponent('down'+pnlL.Tag.ToString) as TSpeedButton);
    btn.Left:=pnlL.Width-(btn.Width);
    btnU.Left:=btn.Left-(btn.Width)-5;
    btnD.Left:=btnU.Left-(btnU.Width)-15;
    if pnl.Tag=pnlL.Tag then
    begin
      btn.Visible:=True;
      btnU.Visible:=True;
      btnD.Visible:=True;
    end
    else
    begin
      btn.Visible:=False;
      btnU.Visible:=False;
      btnD.Visible:=False;
    end;
  end;
end;

procedure TfrmMain.pnlMouseLeave(Sender: TObject);
var
  pnl : TPanel;
begin
  pnl:=(Sender as TLabel).Parent as TPanel;
  pnl.Color:=clBtnFace;
end;

procedure TfrmMain.btnBackClick(Sender: TObject);
begin
  btnEdit.Tag:=-1;
  pnlProjects.Visible:=False;
  pnlRecent.Visible:=False;
  lblFavoriteName.Visible:=False;

  for var I := ProjectList.Count-1 downto 0 do
    ProjectList.Items[I].Free;
  ProjectList.Clear;

  pnlList.Visible:=True;
  btnBack.Visible:=False;
  btnEdit.Visible:=False;
  btnNew.Visible:=True;
  btnRecent.Visible:=True;
end;

procedure TfrmMain.btnEditClick(Sender: TObject);
var
  EditFavorite : TfrmNewFavorite;
begin
  EditFavorite := TfrmNewFavorite.Create(Self);
  try
    TSingletonSettings.RegisterFormClassForTheming(TfrmNewFavorite,EditFavorite);
    EditFavorite.Position:=poMainFormCenter;
    EditFavorite.FavoriteId:=btnEdit.Tag;
    EditFavorite.edtFavoriteName.Text:=lblFavoriteName.Caption;
    EditFavorite.ShowModal;
    for var I := ProjectList.Count-1 downto 0 do
      ProjectList.Items[I].Free;
    ProjectList.Clear;
    loadProjectList(btnEdit.Tag);
  finally
    EditFavorite.Free;
  end;
end;

procedure TfrmMain.btnNewClick(Sender: TObject);
var
  NewFavorite : TfrmNewFavorite;
begin
  NewFavorite := TfrmNewFavorite.Create(Self);
  try
    TSingletonSettings.RegisterFormClassForTheming(TfrmNewFavorite,NewFavorite);
    NewFavorite.Position:=poMainFormCenter;
    NewFavorite.FavoriteId:=-1;
    NewFavorite.ShowModal;
    for var I := FavoriteList.Count-1 downto 0 do
      FavoriteList.Items[I].Free;
    FavoriteList.Clear;
    loadFavoriteList;
  finally
    NewFavorite.Free;
  end;
end;

procedure TfrmMain.btnRecentClick(Sender: TObject);
var
  RecentProjects : TStringList;
  lbl : TLabel;
  pnl : TPanel;
begin
  btnRecent.Visible:=False;
  btnBack.Visible:=True;
  btnNew.Visible:=False;

  lblFavoriteName.Caption:='Recent Projects';
  lblFavoriteName.Visible:=True;

  RecentProjects:=TStringList.Create;
  try
    GetRecentProjects(RecentProjects);

    for var Item in sbRecent do
      Item.Free;

    for var I:=0 to RecentProjects.Count-1 do
    begin
      pnl:=TPanel.Create(sbRecent);
      pnl.BevelOuter:=bvLowered;
      pnl.Top:=999999;
      pnl.Align:=alTop;
      pnl.Height:=40;
      pnl.ShowCaption:=False;
      pnl.Margins.Top:=1;
      pnl.Margins.Right:=2;
      pnl.Margins.Bottom:=2;
      pnl.Margins.Left:=2;
      pnl.AlignWithMargins:=True;
      pnl.ParentColor:=False;
      pnl.ParentBackground:=False;
      pnl.Parent:=sbRecent;

      lbl:=TLabel.Create(pnl);
      lbl.Caption:=ExtractFileName(RecentProjects[I]);
      lbl.Parent:=pnl;
      lbl.Top:=999999;
      lbl.Hint:=RecentProjects[I];
      lbl.Align:=alTop;
      lbl.Font.Style:=[TFontStyle.fsBold];
      lbl.AutoSize:=True;
      lbl.WordWrap:=True;
      lbl.Margins.Top:=3;
      lbl.Margins.Right:=3;
      lbl.Margins.Bottom:=3;
      lbl.Margins.Left:=10;
      lbl.AlignWithMargins:=True;
      lbl.Cursor:=crHandPoint;
      lbl.OnClick:=ProjectClick;
      lbl.OnMouseEnter:=MouseEnter;
      lbl.OnMouseLeave:=MouseLeave;

      lbl:=TLabel.Create(pnl);
      lbl.Caption:=ExtractFilePath(RecentProjects[I]);
      lbl.Parent:=pnl;
      lbl.Top:=999999;
      lbl.Align:=alTop;
      lbl.AutoSize:=True;
      lbl.WordWrap:=True;
      lbl.Margins.Top:=3;
      lbl.Margins.Right:=3;
      lbl.Margins.Bottom:=3;
      lbl.Margins.Left:=15;
      lbl.AlignWithMargins:=True;

      ProjectList.Add(pnl);
    end;

    sbRecent.VertScrollBar.Increment:=pnl.Height;
    sbRecent.Tag:=pnl.Height;
    sbRecent.OnMouseWheel:=MouseWheel;

    pnlRecent.Visible:=True;
    pnlList.Visible:=False;
  finally
    RecentProjects.Free;
  end;
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  loadFavoriteList;
end;

procedure TfrmMain.DelFavoriteClick(Sender: TObject);
begin
  var FavName:=((Sender as TSpeedButton).Parent as TPanel).Caption;
  if MessageDlg('Are you sure you want to delete "'+FavName+'"?',TMsgDlgType.mtConfirmation,mbYesNo,0)=mrYes then
  begin
    DeleteFavorite((Sender as TSpeedButton).Tag.ToString);
    for var I := FavoriteList.Count-1 downto 0 do
      FavoriteList.Items[I].Free;
    FavoriteList.Clear;
    loadFavoriteList;
  end;
end;

destructor TfrmMain.Destroy;
begin
  inherited;
end;

procedure TfrmMain.downClick(Sender: TObject);
var
  Favorites : TStringList;
  sind : Integer;
  svalue : Integer;
begin
  Favorites:=TStringList.Create;
  try
    GetFavoriteList(Favorites);
    Favorites.CustomSort(@CompareFavorites);
    var FavID:=(Sender as TSpeedButton).Tag;
    for var I := 0 to Favorites.Count-1 do
    begin
      var FavItem , nFavItem : TFavItem;
      FavItem := TFavItem(Favorites.Objects[I]);
      if (FavItem.FavoriteID.ToInteger=FavID) and (I<>Favorites.Count-1) then
      begin
        nFavItem := TFavItem(Favorites.Objects[I+1]);
        UpdateFavorite(FavItem.FavoriteID,IntToStr(I+1),FavItem.FavoriteName);
        UpdateFavorite(nFavItem.FavoriteID,I.ToString,nFavItem.FavoriteName);
        FavItem.Free;
        nFavItem.Free;
        for var J := FavoriteList.Count-1 downto 0 do
          FavoriteList.Items[J].Free;
        FavoriteList.Clear;
        loadFavoriteList;
        Break;
      end;
    end;
  finally
    Favorites.Free;
  end;
end;

procedure TfrmMain.FavoriteClick(Sender: TObject);
begin
  pnlList.Visible:=False;
  btnNew.Visible:=False;

  lblFavoriteName.Caption:=(Sender as TLabel).Caption;
  lblFavoriteName.Visible:=True;
  btnEdit.Tag:=(Sender as TLabel).Tag;

  loadProjectList(btnEdit.Tag);

  pnlProjects.Visible:=True;
  btnEdit.Visible:=True;
  btnBack.Visible:=True;
  btnRecent.Visible:=False;
end;

procedure TfrmMain.loadFavoriteList;
var
  lbl : TLabel;
  pnl : TPanel;
  btnDel : TSpeedButton;
  btnUp : TSpeedButton;
  btnDown : TSpeedButton;
  Favorites : TStringList;
begin
  Favorites:=TStringList.Create;
  try
    GetFavoriteList(Favorites);
    Favorites.CustomSort(@CompareFavorites);

    for var I:=0 to Favorites.Count-1 do
    begin
      var FavItem: TFavItem;
      FavItem := TFavItem(Favorites.Objects[I]);
      pnl:=TPanel.Create(sbList);
      pnl.BevelOuter:=bvLowered;
      pnl.Top:=Integer.MaxValue;
      pnl.Height:=30;
      pnl.Top:=I*pnl.Height;
      pnl.Align:=alTop;
      pnl.Width:=sbList.ClientWidth;
      pnl.ShowCaption:=False;
      pnl.Caption:=FavItem.FavoriteName;
      pnl.Margins.Top:=1;
      pnl.Margins.Right:=2;
      pnl.Margins.Bottom:=2;
      pnl.Margins.Left:=2;
      pnl.AlignWithMargins:=True;
      pnl.ParentColor:=False;
      pnl.Tag:=FavItem.FavoriteID.ToInteger;
      pnl.Parent:=sbList;

      btnDel:=TSpeedButton.Create(pnl);
      btnDel.Images:=imgl;
      btnDel.ImageIndex:=0;
      btnDel.Visible:=False;
      btnDel.Left:=pnl.Width-(btnDel.Width);
      btnDel.Align:=alRight;
      btnDel.Width:=pnl.Height;
      btnDel.Name:='Del'+FavItem.FavoriteID;
      btnDel.Tag:=FavItem.FavoriteID.ToInteger;
      btnDel.OnClick:=DelFavoriteClick;
      btnDel.Parent:=pnl;

      btnUp:=TSpeedButton.Create(pnl);
      btnUp.Images:=imgl;
      btnUp.ImageIndex:=1;
      btnUp.Visible:=False;
      btnUp.Left:=btnDel.Left-(btnDel.Width)-5;
      btnUp.Align:=alRight;
      btnUp.Width:=pnl.Height;
      btnUp.Name:='Up'+FavItem.FavoriteID;
      btnUp.Tag:=FavItem.FavoriteID.ToInteger;
      btnUp.OnClick:=upClick;
      btnUp.Parent:=pnl;

      btnDown:=TSpeedButton.Create(pnl);
      btnDown.Images:=imgl;
      btnDown.ImageIndex:=2;
      btnDown.Visible:=False;
      btnDown.Left:=btnUp.Left-(btnUp.Width)-15;
      btnDown.Align:=alRight;
      btnDown.Width:=pnl.Height;
      btnDown.Name:='Down'+FavItem.FavoriteID;
      btnDown.Tag:=FavItem.FavoriteID.ToInteger;
      btnDown.OnClick:=downClick;
      btnDown.Parent:=pnl;

      lbl:=TLabel.Create(pnl);
      lbl.Caption:=FavItem.FavoriteName;
      lbl.Tag:=StrToInt(FavItem.FavoriteID);
      lbl.Parent:=pnl;
      lbl.Align:=alClient;
      lbl.Layout:=tlCenter;
      lbl.Font.Size:=10;
      lbl.Margins.Top:=3;
      lbl.Margins.Right:=3;
      lbl.Margins.Bottom:=3;
      lbl.Margins.Left:=10;
      lbl.AlignWithMargins:=True;
      lbl.Cursor:=crHandPoint;
      lbl.OnClick:=FavoriteClick;
      lbl.Transparent:=True;
      lbl.OnMouseEnter:=pnlMouseEnter;
      lbl.OnMouseLeave:=pnlMouseLeave;
      FavoriteList.Add(pnl);
    end;

    sbList.VertScrollBar.Increment:=pnl.Height;
    sbList.Tag:=pnl.Height;
    sbList.OnMouseWheel:=MouseWheel;
  finally
    Favorites.Free;
  end;
end;

procedure TfrmMain.loadProjectList(FavoriteId: Integer);
var
  lbl : TLabel;
  Projects: TStringList;
  pnl : TPanel;
begin
  Projects:=TStringList.Create;
  try
    if LoadFromFavorite(FavoriteId.ToString,Projects) then
    begin
      for var I:=0 to Projects.Count-1 do
      begin
        pnl:=TPanel.Create(sbProjects);
        pnl.BevelOuter:=bvLowered;
        pnl.Top:=999999;
        pnl.Align:=alTop;
        pnl.Height:=40;
        pnl.ShowCaption:=False;
        pnl.Margins.Top:=1;
        pnl.Margins.Right:=2;
        pnl.Margins.Bottom:=2;
        pnl.Margins.Left:=2;
        pnl.AlignWithMargins:=True;
        pnl.ParentColor:=False;
        pnl.ParentBackground:=False;
        pnl.Parent:=sbProjects;

        lbl:=TLabel.Create(pnl);
        lbl.Caption:=ExtractFileName(Projects[I]);
        lbl.Parent:=pnl;
        lbl.Top:=999999;
        lbl.Hint:=Projects[I];
        lbl.Align:=alTop;
        lbl.Font.Style:=[TFontStyle.fsBold];
        lbl.AutoSize:=True;
        lbl.WordWrap:=True;
        lbl.Margins.Top:=3;
        lbl.Margins.Right:=3;
        lbl.Margins.Bottom:=3;
        lbl.Margins.Left:=10;
        lbl.AlignWithMargins:=True;
        lbl.Cursor:=crHandPoint;
        lbl.OnClick:=ProjectClick;
        lbl.OnMouseEnter:=MouseEnter;
        lbl.OnMouseLeave:=MouseLeave;

        lbl:=TLabel.Create(pnl);
        lbl.Caption:=ExtractFilePath(Projects[I]);
        lbl.Parent:=pnl;
        lbl.Top:=999999;
        lbl.Align:=alTop;
        lbl.AutoSize:=True;
        lbl.WordWrap:=True;
        lbl.Margins.Top:=3;
        lbl.Margins.Right:=3;
        lbl.Margins.Bottom:=3;
        lbl.Margins.Left:=15;
        lbl.AlignWithMargins:=True;

        ProjectList.Add(pnl);
      end;
    end;

    sbProjects.VertScrollBar.Increment:=pnl.Height;
    sbProjects.Tag:=pnl.Height;
    sbProjects.OnMouseWheel:=MouseWheel;
  finally
    Projects.Free;
  end;
end;

procedure TfrmMain.MouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Style:=[TFontStyle.fsBold,TFontStyle.fsUnderline];
end;

procedure TfrmMain.MouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Style:=[TFontStyle.fsBold];
end;

procedure TfrmMain.ProjectClick(Sender: TObject);
begin
  OpenDelphiProject((Sender as TLabel).Hint);
end;

procedure TfrmMain.upClick(Sender: TObject);
var
  Favorites : TStringList;
  sind : Integer;
  svalue : Integer;
begin
  Favorites:=TStringList.Create;
  try
    GetFavoriteList(Favorites);
    Favorites.CustomSort(@CompareFavorites);
    var FavID:=(Sender as TSpeedButton).Tag;
    for var I := 0 to Favorites.Count-1 do
    begin
      var FavItem , pFavItem : TFavItem;
      FavItem := TFavItem(Favorites.Objects[I]);
      if (FavItem.FavoriteID.ToInteger=FavID) and (I<>0) then
      begin
        pFavItem := TFavItem(Favorites.Objects[I-1]);
        UpdateFavorite(FavItem.FavoriteID,IntToStr(I-1),FavItem.FavoriteName);
        UpdateFavorite(pFavItem.FavoriteID,I.ToString,pFavItem.FavoriteName);
        FavItem.Free;
        pFavItem.Free;
        for var J := FavoriteList.Count-1 downto 0 do
          FavoriteList.Items[J].Free;
        FavoriteList.Clear;
        loadFavoriteList;
        Break;
      end;
    end;
  finally
    Favorites.Free;
  end;
end;

procedure TfrmMain.MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    (Sender as TScrollBox).VertScrollBar.Position := (Sender as TScrollBox).VertScrollBar.Position - (Sender as TScrollBox).Tag
  else
    (Sender as TScrollBox).VertScrollBar.Position := (Sender as TScrollBox).VertScrollBar.Position + (Sender as TScrollBox).Tag;
  Handled := True;
end;

initialization
  FavoriteList:=TList<TPanel>.Create;
  ProjectList:=TList<TPanel>.Create;

finalization
  var I : Integer;
  if ProjectList<>nil then
  begin
    if ProjectList.Count>0 then
    begin
      ProjectList.Clear;
    end;
    FreeAndNil(ProjectList);
  end;

  if FavoriteList<>nil then
  begin
    if FavoriteList.Count>0 then
    begin
      FavoriteList.Clear;
    end;
    FreeAndNil(FavoriteList);
  end;

end.
