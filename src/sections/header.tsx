import { divRef, section } from "../types/types";

type HeaderSectionProps = {
  sections: Array<section>;
  executeScroll: (ref: divRef) => () => void;
  openMenu: () => void;
};

export const HeaderSection = ({ sections, executeScroll, openMenu }: HeaderSectionProps) => (
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
        <a className="show-main-menu-link mobile-only" onClick={openMenu}>
          SHOW MAIN MENU
        </a>
      </div>
    </div>
  </header>
);
