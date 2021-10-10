import { divRef, section } from "../types/types";

type HeaderSectionProps = {
  sections: Array<section>;
  toggleMainMenu: (showMainMenu: boolean) => void;
  showMainMenu: boolean;
  executeScroll: (ref: divRef) => () => void;
};

export const HeaderSection = ({
  sections,
  toggleMainMenu,
  showMainMenu,
  executeScroll,
}: HeaderSectionProps) => (
  <header>
    <div id="navbar-padding" />
    <div id="navbar" className="App-header">
      <div className="navbar-inner">
        <div className="max-width-container margin-lr-auto desktop-only centre-row-flex">
          {sections.map((section) => (
            <div className="navbar-link" key={section.title} onClick={executeScroll(section.ref)}>
              {section.title}
            </div>
          ))}
        </div>
        <div
          className="show-main-menu-link mobile-only"
          onClick={() => toggleMainMenu(showMainMenu)}
        >
          <div className="menu-text">Menu</div>
          <svg
            width="150"
            height="150"
            viewBox="0 0 150 150"
            fill="none"
            xmlns="http://www.w3.org/2000/svg"
            id="main-menu-icon"
            className={showMainMenu ? "close-menu" : "open-menu"}
          >
            <path
              id="background-circle"
              d="M150 75C150 116.421 116.421 150 75 150C33.5786 150 0 116.421 0 75C0 33.5786 33.5786 0 75 0C116.421 0 150 33.5786 150 75Z"
              fill="white"
            />
            <line
              id="bottom-line"
              x1="30"
              y1="105"
              x2="120"
              y2="105"
              stroke="black"
              strokeWidth="10"
            />
            <line
              id="middle-line"
              x1="30"
              y1="75"
              x2="120"
              y2="75"
              stroke="black"
              strokeWidth="10"
            />
            <line id="top-line" x1="30" y1="45" x2="120" y2="45" stroke="black" strokeWidth="10" />
          </svg>
        </div>
      </div>
    </div>
  </header>
);
