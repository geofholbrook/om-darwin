let currentFileName = 'Untitled';
        let fileContent = '';

        function updateFilename(newName) {
            currentFileName = newName;
            document.getElementById('filename').innerText = newName;
        }

        function newFile() {
            if (confirm('Are you sure you want to create a new file? Unsaved changes will be lost.')) {
                updateFilename('Untitled');
                document.getElementById('codeEditor').value = '';
            }
        }

        function openFile() {
            const newFileName = prompt('Enter file name:', currentFileName);
            
            fetch(`http://localhost:32794/open/${newFileName}`, {
                method: 'GET',
            })
            .then(response => response.json())
            .then(data => {
                updateFilename(data.filename);
                document.getElementById('codeEditor').value = data.content;
            })
            .catch(error => console.error('Error:', error));
        }

        function saveFile() {
            const content = document.getElementById('codeEditor').value;
            fetch('http://localhost:32794/save', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ filename: currentFileName, content: content }),
            })
            .then(response => response.json())
            .then(data => {
                alert('File saved successfully');
            })
            .catch(error => console.error('Error:', error));
        }

        function saveAsFile() {
            const newFileName = prompt('Enter new file name:', currentFileName);
            if (newFileName) {
                const content = document.getElementById('codeEditor').value;
                fetch('http://localhost:32794/saveAs', {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/json',
                    },
                    body: JSON.stringify({ filename: newFileName, content: content }),
                })
                .then(response => response.json())
                .then(data => {
                    updateFilename(newFileName);
                    alert('File saved as ' + newFileName);
                })
                .catch(error => console.error('Error:', error));
            }
        }