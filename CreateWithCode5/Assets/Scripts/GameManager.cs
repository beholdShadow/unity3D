using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
using UnityEngine.SceneManagement;
using UnityEngine.UI;

public class GameManager : MonoBehaviour
{
    // Start is called before the first frame update
    public List<GameObject> targets;
    public TextMeshProUGUI scoreText;
    public TextMeshProUGUI gameOverText;
    public GameObject titleScreen;
    public Button restartButton;
    public bool IsGameOver { get; set; }

    private int score = 0;
    private float spawnRate = 1.0f;
    void Start()
    {
    }

    public void GameOver()
    {
        IsGameOver = true;
        gameOverText.gameObject.SetActive(true);
        restartButton.gameObject.SetActive(true);
    }

    // Update is called once per frame
    void Update()
    {
        
    }

    public void updateScore(int scoreIncrement) {
        score += scoreIncrement;
        scoreText.SetText("score:" + score);
    }
    public void RestartGame()
    {
        SceneManager.LoadScene(SceneManager.GetActiveScene().name);
        gameOverText.gameObject.SetActive(false);
        restartButton.gameObject.SetActive(false);
    }

    IEnumerator SpawnTarget() {
        while (!IsGameOver) {
            yield return new WaitForSeconds(spawnRate);

            Instantiate(targets[Random.Range(0, targets.Count)]);
        }
    }

    public void startGame(int difficulty) {
        spawnRate = spawnRate / difficulty;
        StartCoroutine(SpawnTarget());
        IsGameOver = false;
        titleScreen.SetActive(false);
    }
}
