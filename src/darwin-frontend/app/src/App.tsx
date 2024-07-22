import styled from "@emotion/styled";
import { Landing } from "./pages";

const App = () => {
  return (
    <AppWrapper>
      <Landing />
    </AppWrapper>
  );
};

export default App;

const AppWrapper = styled.div`
  background: #f4f4f4;
  width: 100%;
  height: 100vh;
`;
