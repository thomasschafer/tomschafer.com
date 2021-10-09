export type technologyType = {
  name: string;
  description: string;
  logoPath: string;
};

export const technologies: Array<technologyType> = [
  { name: "Python", description: "Experience: 2 years", logoPath: "images/python_logo.png" },
  { name: "Django", description: "Experience: 1 year", logoPath: "images/django_logo.jpeg" },
  { name: "SQL", description: "Experience: 2 years", logoPath: "images/postgresql_logo.png" },
  {
    name: "JavaScript and TypeScript",
    description: "Experience: 1 year",
    logoPath: "images/js_ts_logo.jpeg",
  },
  { name: "React", description: "Experience: 1 year", logoPath: "images/react_logo.svg" },
];
