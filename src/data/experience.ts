import { v4 as uuidv4 } from "uuid";

export type experienceType = {
  title: string;
  subtitle: string;
  description: Array<string>;
  type: string;
  logoPath: string;
  experienceId: string;
};

export const workData: Array<experienceType> = [
  {
    title: "Software Engineer",
    subtitle: "Lambert Labs",
    description: [
      "Software development for a variety of clients with varied technology stacks, predominantly using Python and TypeScript. Highlights include:",
      "Integrated React with an existing Django site for an education client to build a dynamic user information dashboard, and implemented caching to dramatically reduce API calls and improve performance",
      "Built various REST endpoints in Django to communicate with React frontends, with a focus on minimising load on the database",
      "Rebuilt various parts a legal client's website as part of a site redesign, using a combination of Django templating, vanilla JavaScript, and React with Typescript",
      "Communicating with multiple stakeholders from across the client businesses and external agencies to understand requirements, propose ideas and demonstrate solutions",
    ],
    type: "job",
    logoPath: "images/lambert_labs_logo.jpeg",
    experienceId: uuidv4(),
  },
  {
    title: "Data Analyst",
    subtitle: "Play Sports Network",
    description: [
      "Using Python and SQL to perform analysis for colleagues across the business, helping inform app and social media strategies. Highlights include:",
      "Analysing comment sentiment towards brands using Google’s NLP API in Python, boosting sponsor engagement and retention",
      "Using the Python library Keras to build models predicting the success of videos based on the thumbnail, helping inform thumbnail design",
    ],
    type: "job",
    logoPath: "images/play_sports_network_logo.png",
    experienceId: uuidv4(),
  },
  {
    title: "Mathematics Research Intern",
    subtitle: "University of Bristol",
    description: [
      "",
      "Studying dynamical systems, specifically orbits in quotient spaces",
      "Working with a researcher to develop and prove hypotheses, and presenting to an audience of academics and students",
    ],
    type: "job",
    logoPath: "images/university_of_bristol_logo.jpeg",
    experienceId: uuidv4(),
  },
];

export const educationData: Array<experienceType> = [
  {
    title: "BSc Mathematics",
    subtitle: "University of Bristol",
    description: [
      "First class honours (final grade 76%), including 89% in final year project on p-adic numbers.",
    ],
    type: "education",
    logoPath: "images/university_of_bristol_logo.jpeg",
    experienceId: uuidv4(),
  },
];
