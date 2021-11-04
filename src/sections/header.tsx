type MenuIconProps = {
  showMainMenu: boolean;
  mainMenuIsTransparent: boolean;
  toggleMainMenu: (showMainMenu: boolean) => void;
};

const MenuIcon = ({ showMainMenu, mainMenuIsTransparent, toggleMainMenu }: MenuIconProps) => (
  <div className="show-main-menu-link" onClick={() => toggleMainMenu(showMainMenu)}>
    <div className="menu-text">Menu</div>
    <svg
      width="150"
      height="150"
      viewBox="0 0 150 150"
      fill="none"
      xmlns="http://www.w3.org/2000/svg"
      id="main-menu-icon"
      className={mainMenuIsTransparent ? "open-menu" : "close-menu"}
    >
      <path
        id="background-circle"
        d="M150 75C150 116.421 116.421 150 75 150C33.5786 150 0 116.421 0 75C0 33.5786 33.5786 0 75 0C116.421 0 150 33.5786 150 75Z"
        fill="white"
      />
      <line id="bottom-line" x1="30" y1="105" x2="120" y2="105" stroke="black" strokeWidth="10" />
      <line id="middle-line" x1="30" y1="75" x2="120" y2="75" stroke="black" strokeWidth="10" />
      <line id="top-line" x1="30" y1="45" x2="120" y2="45" stroke="black" strokeWidth="10" />
    </svg>
  </div>
);

type HeaderSectionProps = {
  showMainMenu: boolean;
  mainMenuIsTransparent: boolean;
  toggleMainMenu: (showMainMenu: boolean) => void;
};

export const HeaderSection = ({
  showMainMenu,
  mainMenuIsTransparent,
  toggleMainMenu,
}: HeaderSectionProps) => (
  <header>
    <div id="navbar-padding" />
    <div id="navbar" className="App-header">
      <div className="navbar-inner max-width-container margin-lr-auto">
        <div className="site-logo">
          <a href="/">
            <img src="tomschafer_logo_192.png" alt="Site logo" />
          </a>
        </div>
        <MenuIcon
          showMainMenu={showMainMenu}
          mainMenuIsTransparent={mainMenuIsTransparent}
          toggleMainMenu={toggleMainMenu}
        />
      </div>
    </div>
  </header>
);
