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
          <p>SHOW MAIN MENU</p>
        </div>
      </div>
    </div>
  </header>
);
