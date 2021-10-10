import { divRef, section } from "./types/types";

type MainMenuProps = {
  sections: Array<section>;
  toggleMainMenu: (showMainMenu: boolean) => void;
  executeScroll: (ref: divRef) => () => void;
};

export const MainMenu = ({ sections, toggleMainMenu, executeScroll }: MainMenuProps) => {
  const jumpToSection = (ref: divRef) => {
    toggleMainMenu(true);
    executeScroll(ref)();
  };

  return (
    <div id="main-menu-modal">
      <div className="main-menu-modal-inner">
        {sections.map((section) => (
          <div
            className="navbar-link large-text"
            key={section.title}
            onClick={() => jumpToSection(section.ref)}
          >
            {section.title}
          </div>
        ))}
      </div>
    </div>
  );
};
