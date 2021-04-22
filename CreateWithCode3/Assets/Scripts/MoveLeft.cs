using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MoveLeft : MonoBehaviour
{
    public float speed;
    private float leftBound = -10;
    private PlayerController playerControllerScript;    
    // Start is called before the first frame update
    void Start()
    {
        playerControllerScript = GameObject.Find("Player").GetComponent<PlayerController>();
    }

    // Update is called once per frame
    void Update()
    {
        if (transform.position.x < leftBound && gameObject.CompareTag("Obstacle"))
            Destroy(gameObject);

        if(!playerControllerScript.gameOver)
            transform.Translate(Vector3.left * Time.deltaTime * speed);  
    }
}
