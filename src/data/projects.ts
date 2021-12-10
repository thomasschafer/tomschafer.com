import { v4 as uuidv4 } from "uuid";

export type projectType = {
  title: string;
  subtitle: string;
  link: string;
  githubLinks?: Array<string>;
  description: Array<string>;
  imagePath: string;
  projectId: string;
};

export const projectData: Array<projectType> = [
  {
    title: "kobble.io",
    subtitle: "Chat application",
    link: "https://kobble.io",
    githubLinks: [
      "https://github.com/thomasschafer/chat-app-frontend",
      "https://github.com/thomasschafer/chat-app-backend",
    ],
    description: [
      "Backend built using Node.js with TypeScript",
      "Frontend built using React with TypeScript, and styled using TailwindCSS",
      "Backend hosted on AWS EC2, utilising Docker and Nginx",
      "Frontend hosted on AWS S3 using CloudFront",
    ],
    imagePath: "images/kobble_screenshot.png",
    projectId: uuidv4(),
  },
  {
    title: "tomschafer.com",
    subtitle: "Personal website",
    link: "https://tomschafer.com",
    githubLinks: ["https://github.com/thomasschafer/personal-website"],
    description: ["Built using React and Typescript", "Styled using SCSS", "Hosted on Netlify"],
    imagePath: "images/personal_site_screenshot.png",
    projectId: uuidv4(),
  },
];
