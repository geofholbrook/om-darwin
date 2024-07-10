
import express from 'express';
import fs from 'fs';

const app = express();

const repository: Record<string, string> = {
    "some-text": "sososomememmee texxxxxt in memory",
    "some-other-text": "sososomememmee oththhteer texxxxxt in memory",
}

app.get('/', (req, res) => {
    const filename = req.query.filename as string;
    // const contents = fs.readFileSync(`/Users/geof/Desktop/${filename}`).toString();
    const contents = repository[filename];
    console.log(contents);
    res.json({ contents });
})

app.listen(5555);
console.log('listening on port 5555')