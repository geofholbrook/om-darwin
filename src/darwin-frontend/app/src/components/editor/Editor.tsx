import styled from "@emotion/styled";

export const Editor = () => {
  return (
    <EditorWrapper>
      <StyledHeader>
        <h1>Untitled</h1>
        <ButtonContainer>
          <StyledButton>New</StyledButton>
          <StyledButton>Open</StyledButton>
          <StyledButton>Save</StyledButton>
          <StyledButton>Save As</StyledButton>
          <StyledButton>List Functions</StyledButton>
        </ButtonContainer>
      </StyledHeader>
      <>
        <StyledTextarea
          id="codeEditor"
          placeholder="Write your JavaScript code here..."
        />
      </>
    </EditorWrapper>
  );
};

const EditorWrapper = styled.div`
  max-width: 1700px;
  margin: 0 auto;
  padding: 0 40px;
`;

const StyledHeader = styled.header`
  background: #f4f4f4;
  padding: 10px;
  display: flex;
  justify-content: space-between;
  align-items: center;
`;

const StyledTextarea = styled.textarea`
  width: 100%;
  min-height: 500px;
`;

const ButtonContainer = styled.div`
  display: flex;
  gap: 10px;
`;

const StyledButton = styled.button`
  padding: 10px;
  font-size: 1em;
`;
